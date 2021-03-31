(ns cnrepl.util.completion
  "Code completion functionality.

  The functionality here is experimental and
  the API is subject to changes."
  {:author "Bozhidar Batsov"
   :added  "0.8"}
  (:require [clojure.main]
            [cnrepl.misc :as misc])                                 ;;; nrepl
  (:import #?@(:clj
               [[java.util.jar JarFile JarEntry]
                [java.io File]
                [java.lang.reflect Field Member]
                [java.util.concurrent ConcurrentHashMap]]
               :cljr
               [[System.Reflection BindingFlags MethodInfo MemberInfo PropertyInfo]])
           ))

;; Code adapted from Compliment (https://github.com/alexander-yakushev/compliment)

(defn annotate-keyword
  [kw]
  {:candidate kw :type :keyword})

(defn all-keywords
  []
  #?(:clj
     (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
       (.setAccessible field true)
       (.keySet ^ConcurrentHashMap (.get field nil)))
     :cljr
     (let [skmInfo 
           (.GetField clojure.lang.Keyword
		                  "_symKeyMap"
					            (enum-or  BindingFlags/Static
					                      BindingFlags/NonPublic))
           skm
		       (.GetValue skmInfo nil)
		       dictInfo
		       (.GetField (class skm)
		                  "_dict"
					            (enum-or System.Reflection.BindingFlags/NonPublic
					                     System.Reflection.BindingFlags/Instance))
		       dict
		       (.GetValue dictInfo skm)]
	     (.Keys dict))))

(defn- resolve-namespace
  [sym ns]
  (get (ns-aliases ns) sym (find-ns sym)))

(defn qualified-auto-resolved-keywords
  "Given a namespace alias, a prefix, and a namespace, return completion
  candidates for qualified, auto-resolved keywords (e.g. ::foo/bar)."
  [ns-alias prefix ns]
  (let [ns-alias-name (str (resolve-namespace (symbol ns-alias) ns))]
    (sequence
     (comp
      (filter #(= (namespace %) ns-alias-name))
      (filter #(#?(:clj .startsWith :cljr .StartsWith) (name %) prefix))
      (map #(str "::" ns-alias "/" (name %)))
      (map annotate-keyword))
     (all-keywords))))

(defn unqualified-auto-resolved-keywords
  "Given a prefix and a namespace, return completion candidates for
  keywords that belong to the given namespace."
  [prefix ns]
  (sequence
   (comp
    (filter #(= (namespace %) (str ns)))
    (filter #(#?(:clj .startsWith :cljr .StartsWith) (name %) (subs prefix 2)))
    (map #(str "::" (name %)))
    (map annotate-keyword))
   (all-keywords)))

(defn keyword-namespace-aliases
  "Given a prefix and a namespace, return completion candidates for namespace
  aliases as auto-resolved keywords."
  [prefix ns]
  (sequence
   (comp
    (map (comp name first))
    #?(:clj
       (filter (fn [^String alias-name] (.startsWith alias-name (subs prefix 2))))
       :cljr
       (filter (fn [^String alias-name] (.StartsWith alias-name (subs prefix 2)))))
    (map #(str "::" (name %)))
    (map annotate-keyword))
   (ns-aliases ns)))

(defn single-colon-keywords
  "Given a prefix, return completion candidates for keywords that are either
  unqualified or qualified with a synthetic namespace."
  [prefix]
  (sequence
   (comp
    (filter #(#?(:clj .startsWith :cljr .StartsWith) (str %) (subs prefix 1)))
    (map #(str ":" %))
    (map annotate-keyword))
   (all-keywords)))

(defn keyword-candidates
  [^String prefix ns]
  (assert (string? prefix))
  (let [double-colon? #?(:clj (.startsWith prefix "::") :cljr (.StartsWith prefix "::"))
        single-colon? #?(:clj (.startsWith prefix ":") :cljr (.StartsWith prefix ":"))
        slash-pos #?(:clj (.indexOf prefix "/") :cljr (.IndexOf prefix "/"))]
    (cond
      (and double-colon? (pos? slash-pos))
      (let [ns-alias (subs prefix 2 slash-pos)
            prefix (subs prefix (inc slash-pos))]
        (qualified-auto-resolved-keywords ns-alias prefix ns))

      double-colon?
      (into
       (unqualified-auto-resolved-keywords prefix ns)
       (keyword-namespace-aliases prefix ns))

      single-colon?
      (single-colon-keywords prefix))))

;; Code adapted from clojure-complete (https://github.com/ninjudd/clojure-complete)
;; The results follow compliment's format, as it's richer and gives more useful
;; data to clients.

;;; Utility functions
(defn namespaces
  "Returns a list of potential namespace completions for a given namespace"
  [ns]
  (concat (map ns-name (all-ns)) (keys (ns-aliases ns))))

(defn ns-public-vars
  "Returns a list of potential public var name completions for a given namespace"
  [ns]
  (vals (ns-publics ns)))

(defn ns-vars
  "Returns a list of all potential var name completions for a given namespace"
  [ns]
  (filter var? (vals (ns-map ns))))

(defn ns-classes
  "Returns a list of potential class name completions for a given namespace"
  [ns]
  (keys (ns-imports ns)))

(def special-forms
  '[def if do let quote var fn loop recur throw try monitor-enter monitor-exit dot new set!])

#?(:clj
   (defn- static? [#^java.lang.reflect.Member member]
     (java.lang.reflect.Modifier/isStatic (.getModifiers member)))
   :cljr
   (defn- static? [member] ;;; java.lang.reflect.Member
     (let [member (if (instance? PropertyInfo member) (.GetGetMethod member) member)]
       (.IsStatic member))))

(defn ns-java-methods
  "Returns a list of Java method names for a given namespace."
  [ns]
  #?(:clj
     (distinct                    ; some methods might exist in multiple classes
      (for [class (vals (ns-imports ns))
            method (.getMethods ^Class class)
            :when (static? method)]
        (str "." (.getName ^Member method))))
     :cljr
     (distinct                    ; some methods might exist in multiple classes
      (for [class (vals (ns-imports ns))
            method (.GetMethods ^Type class)
            :when (static? method)]
        (str "." (.Name ^MethodInfo method))))))

#?(:clj
   (defn static-members
     "Returns a list of potential static members for a given class"
     [^Class class]
     (->> (concat (.getMethods class) (.getDeclaredFields class))
          (filter static?)
          (map #(.getName ^Member %))
          distinct))
   :cljr (defn static-members
           "Returns a list of potential static members for a given class"
           [^Type class] 
           (->> (concat (.GetMethods class) (.GetFields class) (.GetProperties class))
                (filter static?)
                (map #(.Name ^MemberInfo %))
                distinct)))

#?(:clj
   (defn path-files [^String path]
     (cond (.endsWith path "/*")
           (for [^File jar (.listFiles (File. path)) :when (.endsWith ^String (.getName jar) ".jar")
                 file (path-files (.getPath jar))]
             file)

           (.endsWith path ".jar")
           (try (for [^JarEntry entry (enumeration-seq (.entries (JarFile. path)))]
                  (.getName entry))
                (catch Exception _e))

           :else
           (for [^File file (file-seq (File. path))]
             (.replace ^String (.getPath file) path ""))))
   :cljr
   (defn path-files [^String path] ;; TODO: needs a complete rethinking for CLR
     ()))

#?(:clj
   (def classfiles
     (for [prop (filter #(System/getProperty %1) ["sun.boot.class.path" "java.ext.dirs" "java.class.path"])
           path (.split (System/getProperty prop) File/pathSeparator)
           ^String file (path-files path) :when (and (.endsWith file ".class") (not (.contains file "__")))]
       file))
   :cljr
   (def classfiles ;; TODO: needs a complete rethinking for CLR
     ()))

#?(:clj
   (defn- classname [^String file]
     (.. file (replace ".class" "") (replace File/separator ".")))
   :cljr
   (defn- classname [^String file] ;; TODO: needs a complete rethinking for CLR
     file))

#?(:clj
   (def top-level-classes
     (future
       (doall
        (for [file classfiles :when (re-find #"^[^\$]+\.class" file)]
          (classname file)))))
   :cljr
   (def top-level-classes ;; TODO: needs a complete rethinking for CLR
     (future ())))

#?(:clj
   (def nested-classes
     (future
       (doall
        (for [file classfiles :when (re-find #"^[^\$]+(\$[^\d]\w*)+\.class" file)]
          (classname file)))))
   :cljr
   (def nested-classes ;; TODO: needs a complete rethinking for CLR
     (future ())))

(defn resolve-class [ns sym]
  (try (let [val (ns-resolve ns sym)]
         (when (class? val) val))
       (catch Exception e
         (when (not= #?(:clj ClassNotFoundException :cljr System.TypeLoadException) ;; not sure what to put here
                     (class (clojure.main/repl-exception e)))
           (throw e)))))

;;; Candidates

(defn annotate-var [var {:keys [extra-metadata]}]
  (let [{macro :macro arglists :arglists var-name :name doc :doc} (-> var meta misc/sanitize-meta)
        type (cond macro :macro
                   arglists :function
                   :else :var)]
    (cond-> {:candidate (name var-name) :type type}
      (and (contains? extra-metadata :doc) doc) (assoc :doc doc)
      (and (contains? extra-metadata :arglists) arglists) (assoc :arglists arglists))))

(defn annotate-class
  [cname]
  {:candidate cname :type :class})

(def special-form-candidates
  (map #(hash-map :candidate (name %) :type :special-form :ns "clojure.core") special-forms))

(defn ns-candidates
  [ns {:keys [extra-metadata]}]
  ;; Calling meta on sym that names a namespace only returns doc if the ns form
  ;; uses the docstring arg, but not if it uses the ^{:doc "..."} meta form.
  ;;
  ;; find-ns returns the namespace the sym names. Calling meta on it returns
  ;; the docstring, no matter which way it's defined.
  (map #(let [doc (some-> % find-ns meta :doc)]
          (cond-> {:candidate (name %)
                   :type :namespace}
            (and (contains? extra-metadata :doc) doc) (assoc :doc doc)))
       (namespaces ns)))

(defn ns-var-candidates
  [ns options]
  (map #(annotate-var % options) (ns-vars ns)))

(defn ns-public-var-candidates
  [ns options]
  (map #(annotate-var % options) (ns-public-vars ns)))

(defn ns-class-candidates
  [ns]
  (map #(hash-map :candidate (name %) :type :class) (ns-classes ns)))

(defn ns-java-method-candidates
  [ns]
  (for [method (ns-java-methods ns)]
    {:candidate method :type :method}))

(defn static-member-candidates
  [class]
  (for [name (static-members class)]
    {:candidate name :type :static-method}))

(defn scoped-candidates
  [^String prefix ns options]
  (when-let [prefix-scope (first
                           (#?(:clj .split :cljr System.Text.RegularExpressions.Regex/Split) prefix "/" ))]
    (let [scope (symbol prefix-scope)]
      (map #(update % :candidate (fn [c] (str scope "/" c)))
           (if-let [class (resolve-class ns scope)]
             (static-member-candidates class)
             (when-let [ns (or (find-ns scope) (scope (ns-aliases ns)))]
               (ns-public-var-candidates ns options)))))))

(defn class-candidates
  [^String prefix ns]
  (map annotate-class
       (if (#?(:clj .contains :cljr .Contains) prefix "+")
         @nested-classes
         @top-level-classes)))

(defn generic-candidates
  [ns options]
  (concat special-form-candidates
          (ns-candidates ns options)
          (ns-var-candidates ns options)
          (ns-class-candidates ns)))

(defn completion-candidates
  [^String prefix ns options]
  (cond
    (#?(:clj .startsWith :cljr .StartsWith) prefix ":") (keyword-candidates prefix ns)
    (#?(:clj .startsWith :cljr .StartsWith) prefix ".") (ns-java-method-candidates ns)
    (#?(:clj .contains :cljr .Contains) prefix "/")     (scoped-candidates prefix ns options)
    (#?(:clj .contains :cljr .Contains) prefix ".")     (concat (ns-candidates ns options) (class-candidates prefix ns))
    :else                                               (generic-candidates ns options)))

(defn completions
  "Return a sequence of matching completion candidates given a prefix string and an optional current namespace."
  ([prefix]
   (completions prefix *ns*))
  ([prefix ns]
   (completions prefix ns nil))
  ([^String prefix ns options]
   (let [candidates (completion-candidates prefix ns options)]
     (sort-by :candidate (filter #(#?(:clj .startsWith :cljr .StartsWith) ^String (:candidate %) prefix) candidates)))))
