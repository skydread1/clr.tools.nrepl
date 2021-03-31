(ns cnrepl.transport-test
  (:require [cnrepl.transport :as sut]
            [clojure.test :refer [deftest testing is]])
  (:import #?(:clj [java.io ByteArrayOutputStream]
              :cljr [System.IO MemoryStream])))

(deftest bencode-safe-write-test
  (testing "safe-write-bencode only writes if the whole message is writable"
    #?(:clj
       (let [out (ByteArrayOutputStream.)]
         (is (thrown? IllegalArgumentException
                      (#'sut/safe-write-bencode out {"obj" (Object.)})))
         (is (empty? (.toByteArray out))))
       :cljr
       (let [out (MemoryStream.)]
         (is (thrown? ArgumentException
                      (#'sut/safe-write-bencode out {"obj" (Object.)})))
         (is (empty? (.ToArray out)))))))
