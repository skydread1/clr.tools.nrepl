(ns cnrepl.middleware.sideloader
  "Support the ability to interactively load resources (including Clojure source
  files) and classes from the client. This can be used to add dependencies to
  the nREPL environment after initial startup."
  {:author "Christophe Grand"
   :added  "0.7"}
  (:require
   #?(:clj [clojure.java.io :as io]
      :cljr [clojure.clr.io :as io])
   [cnrepl.middleware :as middleware :refer [set-descriptor!]]
   [cnrepl.misc :refer [response-for]]
   [cnrepl.transport :as t])
  (:import cnrepl.transport.Transport))
;;;;;;;;;;;;;;;;;;  Making this a no-op for now -- no idea what the equivalent would be in CLR ;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: dedup with base64 in elisions branch once both are merged
#?(:clj
   (defn base64-encode [^java.io.InputStream in]
     (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
           sb (StringBuilder.)]
       (loop [shift 4 buf 0]
         (let [got (.read in)]
           (if (neg? got)
             (do
               (when-not (= shift 4)
                 (let [n (bit-and (bit-shift-right buf 6) 63)]
                   (.append sb (.charAt table n))))
               (cond
                 (= shift 2) (.append sb "==")
                 (= shift 0) (.append sb \=))
               (str sb))
             (let [buf (bit-or buf (bit-shift-left got shift))
                   n (bit-and (bit-shift-right buf 6) 63)]
               (.append sb (.charAt table n))
               (let [shift (- shift 2)]
                 (if (neg? shift)
                   (do
                     (.append sb (.charAt table (bit-and buf 63)))
                     (recur 4 0))
                   (recur shift (bit-shift-left buf 6))))))))))
   :cljr
   (defn base64-encode [^System.IO.Stream in]
     (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
           sb (StringBuilder.)]
       (loop [shift 4 buf 0]
         #_(let [got (.Read in)] ;;; .read :FIXME : failing in Magic
             (if (neg? got)
               (do
                 (when-not (= shift 4)
                   (let [n (bit-and (bit-shift-right buf 6) 63)]
                     (.Append sb (.get_Chars table n))))
                 (cond
                   (= shift 2) (.append sb "==")
                   (= shift 0) (.append sb \=))
                 (str sb))
               (let [buf (bit-or buf (bit-shift-left got shift))
                     n (bit-and (bit-shift-right buf 6) 63)]
                 (.Append sb (.get_Chars table n))
                 (let [shift (- shift 2)]
                   (if (neg? shift)
                     (do
                       (.Append sb (.get_Chars table (bit-and buf 63)))
                       (recur 4 0))
                     (recur shift (bit-shift-left buf 6)))))))))))

(defn base64-decode [^String s]
  #?(:clj
     (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
           in (java.io.StringReader. s)
           bos (java.io.ByteArrayOutputStream.)]
       (loop [bits 0 buf 0]
         (let [got (.read in)]
           (when-not (or (neg? got) (= 61 got))
             (let [buf (bit-or (.indexOf table got) (bit-shift-left buf 6))
                   bits (+ bits 6)]
               (if (<= 8 bits)
                 (let [bits (- bits 8)]
                   (.write bos (bit-shift-right buf bits))
                   (recur bits (bit-and 63 buf)))
                 (recur bits buf))))))
       (.toByteArray bos))
     :cljr
     (let [table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
           in (System.IO.StringReader. s)
           bos (System.IO.MemoryStream.)]
       (loop [bits 0 buf 0]
         (let [got (.Read in)]
           (when-not (or (neg? got) (= 61 got))
             (let [buf (bit-or (.IndexOf table got) (bit-shift-left buf 6))
                   bits (+ bits 6)]
               (if (<= 8 bits)
                 (let [bits (- bits 8)]
                   (.Write bos (bit-shift-right buf bits))
                   (recur bits (bit-and 63 buf)))
                 (recur bits buf))))))
       (.ToArray bos))))

(defn- sideloader
  "Creates a classloader that obey standard delegating policy."
  [{:keys [session id transport] :as msg} pending]
  (fn [] nil))
#_(let [resolve-fn
        (fn [type name]
          (let [p (promise)]
              ;; Swap into the atom *before* sending the lookup request to ensure that the server
              ;; knows about the pending request when the client sends the response.
            (swap! pending assoc [(clojure.core/name type) name] p)
            (t/send transport (response-for msg
                                            {:status :sideloader-lookup
                                             :type type
                                             :name name}))
            @p))]
    (proxy [clojure.lang.DynamicClassLoader] [(.getContextClassLoader (Thread/currentThread))]
      (findResource [name]
        (when-some  [bytes (resolve-fn "resource" name)]
          (let [file (doto (java.io.File/createTempFile "nrepl-sideload-" (str "-" (re-find #"[^/]*$" name)))
                       .deleteOnExit)]
            (io/copy bytes file)
            (-> file .toURI .toURL))))
      (findClass [name]
        (if-some  [bytes (resolve-fn "class" name)]
          (.defineClass ^clojure.lang.DynamicClassLoader this name bytes nil)
          (throw (ClassNotFoundException. name))))))

(defn wrap-sideloader
  "Middleware that enables the client to serve resources and classes to the server."
  [h]
  (let [pending (atom {})]
    (fn [{:keys [op type name content transport session] :as msg}]
      (case op
        "sideloader-start"
        (alter-meta! session assoc :classloader
                     (sideloader msg pending))

        "sideloader-provide"
        (if-some [p (@pending [type name])]
          (do
            (deliver p (let [bytes (base64-decode content)]
                         (when (pos? (count bytes))
                           bytes)))
            (swap! pending dissoc [type name])
            (t/send transport (response-for msg {:status :done})))
          (t/send transport (response-for msg {:status #{:done :unexpected-provide}
                                               :type type
                                               :name name})))

        (h msg)))))

(set-descriptor! #'wrap-sideloader
                 {:requires #{"clone"}
                  :expects #{"eval"}
                  :handles {"sideloader-start"
                            {:doc "Starts a sideloading session."
                             :requires {"session" "the id of the session"}
                             :optional {}
                             :returns {"status" "\"sideloader-lookup\", never ever returns \"done\"."}}
                            "sideloader-provide"
                            {:doc "Provides a requested class or resource."
                             :requires {"session" "the id of the session"
                                        "content" "base64 string"
                                        "type" "\"class\" or \"resource\""
                                        "name" "the class or resource name"}
                             :optional {}
                             :returns {}}}})
