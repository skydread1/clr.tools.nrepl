(ns cnrepl.edn-test
  (:require [clojure.test :refer [deftest is testing]]
            [cnrepl.core :as nrepl]
            [cnrepl.server :as server]
            [cnrepl.transport :as transport]))

(defn return-evaluation
  [message]
  (with-open [server (server/start-server :transport-fn transport/edn)]
    (with-open [conn (nrepl/connect :transport-fn transport/edn
                                    :port (:port server))]
      (-> (nrepl/client conn 1000)
          (nrepl/message message)
          nrepl/response-values))))

(deftest edn-transport-communication
  (testing "op as a string value"
    (is (= (return-evaluation {:op "eval" :code "(+ 2 3)"})
           [5])))
  (testing "op as a keyword value"
    (is (= (return-evaluation {:op :eval :code "(+ 2 3)"})
           [5])))
  (testing "simple expressions"
    (is (= (return-evaluation {:op "eval" :code "(range 40)"})
           [(eval '(range 40))]))))
