(ns cnrepl.transport
  {:author "Chas Emerick"}
  (:refer-clojure :exclude [send])
  (:require
   #?@(:clj
      [[clojure.java.io :as io]]
      :cljr
      [[clojure.clr.io :as io]
       [cnrepl.sync-channel :as sc]]  ;; Added only for .net
)
   [clojure.walk :as walk]
   [cnrepl.bencode :as bencode]
   [clojure.edn :as edn]
   [cnrepl.misc :refer [uuid]]
   cnrepl.version)
  (:import
   #?@(:clj
       [clojure.lang.RT
        [java.io ByteArrayOutputStream EOFException PushbackInputStream PushbackReader OutputStream]
        [java.net Socket SocketException]
        [java.util.concurrent BlockingQueue LinkedBlockingQueue SynchronousQueue TimeUnit]]
       :cljr
       [clojure.lang.RT
        [System.IO Stream EndOfStreamException MemoryStream]
        [clojure.lang PushbackInputStream PushbackTextReader]
        (System.Net.Sockets Socket SocketException)
        [System.Collections.Concurrent |BlockingCollection`1[System.Object]|]])))

(defprotocol Transport
  "Defines the interface for a wire protocol implementation for use
   with nREPL."
  (recv [this] [this timeout]
    "Reads and returns the next message received.  Will block.
     Should return nil the a message is not available after `timeout`
     ms or if the underlying channel has been closed.")
  (send [this msg] "Sends msg. Implementations should return the transport."))

(deftype FnTransport [recv-fn send-fn close]
  Transport
  (send [this msg] (send-fn msg) this)
  (recv [this] (.recv this #?(:clj Long/MAX_VALUE :cljr Int64/MaxValue)))
  (recv [this timeout] (recv-fn timeout))
  #?@(:clj
      [java.io.Closeable
       (close [_this] (close))]
      :cljr
      [System.IDisposable
       (Dispose [this] (close))])) ;; TODO: This violates good IDisposable practice

(defn fn-transport
  "Returns a Transport implementation that delegates its functionality
   to the 2 or 3 functions provided."
  ([transport-read write] (fn-transport transport-read write nil))
  ([transport-read write close]
   #?(:clj
      (let [read-queue (SynchronousQueue.)
            msg-pump (future (try
                               (while true
                                 (.put read-queue (transport-read)))
                               (catch Throwable t
                                 (.put read-queue t))))]
        (FnTransport.
         (let [failure (atom nil)]
           #(if @failure
              (throw @failure)
              (let [msg (.poll read-queue % TimeUnit/MILLISECONDS)]
                (if (instance? Throwable msg)
                  (do (reset! failure msg) (throw msg))
                  msg))))
         write
         (fn [] (close) (future-cancel msg-pump))))
      :cljr
      (let [read-queue (sc/make-simple-sync-channel)
            msg-pump (future (try
                               (while true
                                 (sc/put read-queue (transport-read)))
                               (catch Exception t
                                 (sc/put read-queue t))))]
        (FnTransport.
         (let [failure (atom nil)]
           #(if @failure
              (throw @failure)
              (let [msg (sc/poll read-queue % )]
                (if (instance? Exception msg)
                  (do (reset! failure msg) (throw msg))
                  msg))))
         write
         (fn [] (close) (future-cancel msg-pump)))))))

(defmulti #^{:private true} <bytes class)

(defmethod <bytes :default
  [input]
  input)

#?(:cljr (def #^{:private true} utf8 (System.Text.UTF8Encoding.))) ;; DM:Added

#?(:clj
   (defmethod <bytes (RT/classForName "[B")
     [#^"[B" input]
     (String. input "UTF-8"))
   :cljr (defmethod <bytes |System.Byte[]|
           [#^|System.Byte[]| input]
           (.GetString utf8 input)))

(defmethod <bytes clojure.lang.IPersistentVector
  [input]
  (vec (map <bytes input)))

(defmethod <bytes clojure.lang.IPersistentMap
  [input]
  (->> input
       (map (fn [[k v]] [k (<bytes v)]))
       (into {})))

#?(:clj
   (defmacro ^{:private true} rethrow-on-disconnection
     [^Socket s & body]
     `(try
        ~@body
        (catch RuntimeException e#
          (if (= "EOF while reading" (.getMessage e#))
            (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
            (throw e#)))
        (catch EOFException e#
          (if (= "Invalid netstring. Unexpected end of input." (.getMessage e#))
            (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
            (throw e#)))
        (catch Throwable e#
          (if (and ~s (not (.isConnected ~s)))
            (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
            (throw e#)))))
   :cljr
   (defmacro ^{:private true} rethrow-on-disconnection
     [s & body] ;; remove type ^Socket int as magic weirdly fails
     `(try
        ~@body
        #_(catch RuntimeException e# ;;; I'm not sure what is covered here
            (if (= "EOF while reading" (.Message e#))
              (throw (SocketException. "The transport's socket appears to have lost its connection to the nREPL server"))
              (throw e#)))
        (catch System.IO.EndOfStreamException e#
          (if (= "Invalid netstring. Unexpected end of input." (.Message e#))
            (throw (.Message (SocketException.)))
            (throw e#)))
        (catch Exception e#
          (if (and ~s (not (.Connected ~s)))
            (throw (.Message (SocketException.)))
            (throw e#))))))

(defn ^{:private true} safe-write-bencode
  "Similar to `bencode/write-bencode`, except it will only writes to the output
   stream if the whole `thing` is writable. In practice, it avoids sending partial
    messages down the transport, which is almost always bad news for the client.

   This will still throw an exception if called with something unencodable."
  [output thing]
  (let [buffer #?(:clj (ByteArrayOutputStream.) :cljr (MemoryStream.))]
    (try
      (bencode/write-bencode buffer thing))
    #?(:clj (.write ^OutputStream output (.toByteArray buffer))
       :cljr (.Write ^Stream output (.ToArray buffer)))))

(defn bencode
  "Returns a Transport implementation that serializes messages
   over the given Socket or InputStream/OutputStream using bencode."
  ([^Socket s] (bencode s s s))
  ([in out & [^Socket s]]
   (let [in (PushbackInputStream. (io/input-stream in))
         out (io/output-stream out)]
     (rethrow-on-disconnection s (bencode/read-bencode in))
     (fn-transport
      #(let [payload (rethrow-on-disconnection s (bencode/read-bencode in))
             unencoded (<bytes (payload "-unencoded"))
             to-decode (apply dissoc payload "-unencoded" unencoded)]
         (walk/keywordize-keys (merge (dissoc payload "-unencoded")
                                      (when unencoded {"-unencoded" unencoded})
                                      (<bytes to-decode))))
      #(rethrow-on-disconnection s
                                 (locking out
                                   (doto out
                                     (safe-write-bencode %)
                                     #?(:clj .flush :cljr .Flush))))
      (fn []
        (if s
          #?(:clj (.close s) :cljr (.Close s))
          (do
            #?(:clj (.close in) :cljr (.Close in))
            #?(:clj (.close out) :cljr (.Close out)))))))))

(defn edn
  "Returns a Transport implementation that serializes messages
   over the given Socket or InputStream/OutputStream using EDN."
  {:added "0.7"}
  ([^Socket s] (edn s s s))
  ([in out & [^Socket s]]
   (let [in #?(:clj (java.io.PushbackReader. (io/reader in))
               :cljr (PushbackTextReader. (io/text-reader in)))
         out #?(:clj (io/writer out)
                :cljr (io/text-writer out))]
     (fn-transport
      #(rethrow-on-disconnection s (edn/read in))
      #(rethrow-on-disconnection s
                                 (locking out
                                   ;; TODO: The transport doesn't seem to work
                                   ;; without these bindings. Worth investigating
                                   ;; why
                                   (binding [*print-readably* true
                                             *print-length*   nil
                                             *print-level*    nil]
                                     (doto out
                                       #?(:clj (.write (str %)) :cljr (.Write (str %)))
                                       #?(:clj (.flush) :cljr (.Flush))))))
      (fn []
        (if s
          #?(:clj (.close s) :cljr (.Close s))
          (do
            #?(:clj (.close in) :cljr (.Close in))
            #?(:clj (.close out) :cljr (.Close out)))))))))

(defn tty
  "Returns a Transport implementation suitable for serving an nREPL backend
   via simple in/out readers, as with a tty or telnet connection."
  ([^Socket s] (tty s s s))
  ([in out & [^Socket s]]
   (let [r #?(:clj (java.io.PushbackReader. (io/reader))
              :cljr (PushbackTextReader. (io/text-reader in)))
         w #?(:clj (io/writer out)
              :cljr (io/text-writer out))
         cns (atom "user")
         prompt (fn [newline?]
                  (when newline? (#?(:clj .write :cljr .Write) w (int \newline)))
                  (#?(:clj .write :cljr .Write) w (str @cns "=> ")))
         session-id (atom nil)
         read-msg #(let [code (read r)]
                     (merge {:op "eval" :code [code] :ns @cns :id (str "eval" (uuid))}
                            (when @session-id {:session @session-id})))
         read-seq (atom (cons {:op "clone"} (repeatedly read-msg)))
         write (fn [{:keys [out err value status ns new-session id] :as msg}]
                 (when new-session (reset! session-id new-session))
                 (when ns (reset! cns ns))
                 (doseq [^String x [out err value] :when x]
                   (#?(:clj .write :cljr .Write) w x))
                 (when (and (= status #{:done})
                            id
                            (#?(:clj .startsWith :cljr .StartsWith) ^String id "eval"))
                   (prompt true))
                 (#?(:clj .flush :cljr .Flush) w))
         read #(let [head (promise)]
                 (swap! read-seq (fn [s]
                                   (deliver head (first s))
                                   (rest s)))
                 @head)]
     (fn-transport read write
                   (when s
                     (swap! read-seq (partial cons {:session @session-id :op "close"}))
                     #(#?(:clj (.close s) :cljr (.Close s))))))))

(defn tty-greeting
  "A greeting fn usable with `nrepl.server/start-server`,
   meant to be used in conjunction with Transports returned by the
   `tty` function.

   Usually, Clojure-aware client-side tooling would provide this upon connecting
   to the server, but telnet et al. isn't that."
  [transport]
  (send transport {:out (str ";; nREPL " (:version-string cnrepl.version/version) ;;nrepl
                             \newline
                             ";; Clojure " (clojure-version)
                             \newline
                             "user=> ")}))

(defmulti uri-scheme
  "Return the uri scheme associated with a transport var."
  identity)

(defmethod uri-scheme #'bencode [_] "nrepl")

(defmethod uri-scheme #'tty [_] "telnet")

(defmethod uri-scheme #'edn [_] "nrepl+edn")

(defmethod uri-scheme :default
  [transport]
  (printf "WARNING: No uri scheme associated with transport %s\n" transport)
  "unknown")


(deftype QueueTransport
 #?(:clj  [^BlockingQueue in ^BlockingQueue out]
    :cljr [^System.Object in ;; NOTE : The type hint ^|System.Collections.Concurrent.BlockingCollection`1[System.Object]| makes the reader cond bug
           ^System.Object out])
 cnrepl.transport.Transport
 (send [this msg]
   #?(:clj (.put out msg)
      :cljr (.Add out msg))
   this)
 (recv [_this]
   #?(:clj (.take in)
      :cljr (.Take in)))
 (recv [_this timeout]
   #?(:clj  (.poll in timeout TimeUnit/MILLISECONDS)
      :cljr (let [x nil] (.TryTake in (by-ref x) (int timeout)) x))))

(defn piped-transports
  "Returns a pair of Transports that read from and write to each other."
  []
  (let [a #?(:clj (LinkedBlockingQueue.)
             :cljr (|System.Collections.Concurrent.BlockingCollection`1[System.Object]|.))
        b #?(:clj (LinkedBlockingQueue.)
             :cljr (|System.Collections.Concurrent.BlockingCollection`1[System.Object]|.))]
    [(QueueTransport. a b) (QueueTransport. b a)]))
