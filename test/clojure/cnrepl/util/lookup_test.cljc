(ns cnrepl.util.lookup-test
  (:require [clojure.test :refer :all]
            [cnrepl.bencode :as bencode]
            [cnrepl.util.lookup :as l :refer [lookup]])
  (:import #?(:clj (java.io ByteArrayOutputStream)
              :cljr (System.IO MemoryStream))))

(deftest lookup-test
  (testing "special sym lookup"
    (is (not-empty (lookup 'clojure.core 'if))))

  (testing "fully qualified sym lookup"
    (is (not-empty (lookup 'cnrepl.util.lookup 'clojure.core/map))))

  (testing "aliased sym lookup"
    (is (not-empty (lookup 'cnrepl.util.lookup 'clojure.string/upper-case))))

  (testing "non-qualified lookup"
    (is (not-empty (lookup 'clojure.core 'map)))

    (is (= {:ns "clojure.core"
            :name "map"
            :arglists "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"
            :arglists-str "([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])"}
           (select-keys (lookup 'cnrepl.util.lookup 'map) [:ns :name :arglists :arglists-str])
           (select-keys (lookup 'clojure.core 'map) [:ns :name :arglists :arglists-str]))))

  (testing "macro lookup"
    (is (= {:ns "clojure.core"
            :name "future"
            :macro #?(:clj "true" :cljr "True")}
           (select-keys (lookup 'clojure.core 'future) [:ns :name :macro]))))

  (testing "special form lookup"
    (is (= {:ns "clojure.core"
            :name "let"
            :special-form #?(:clj "true" :cljr "True")}
           (select-keys (lookup 'clojure.core 'let) [:ns :name :special-form]))))

  (testing "Java sym lookup"
    (is (empty? (lookup 'clojure.core 'String)))))

(defn- bencode-str
  "Bencode a thing and write it into a string."
  [thing]
  #?(:clj
     (let [out (ByteArrayOutputStream.)]
       (try
         (bencode/write-bencode out thing)
         (.toString out)
         (catch IllegalArgumentException ex
           (throw (ex-info (.getMessage ex) {:thing thing})))))
     :cljr
     (let [out (MemoryStream.)]
       (try
         (bencode/write-bencode out thing)
         (.ToString out)
         (catch ArgumentException ex
           (throw (ex-info (.Message ex) {:thing thing})))))))

(defn- lookup-public-vars
  "Look up every public var in all namespaces in the classpath and return the result as a set."
  []
  (transduce
   (comp
    (mapcat ns-publics)
    (map (comp meta val))
    (map #(lookup (.getName (:ns %)) (:name %))))
   conj
   #{}
   (all-ns)))

(deftest bencode-test
  (doseq [m (lookup-public-vars)]
    (is ((comp string? not-empty) (bencode-str m)))))
