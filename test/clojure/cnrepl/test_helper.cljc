                                        ;   Copyright (c) Rich Hickey. All rights reserved.
                                        ;   The use and distribution terms for this software are covered by the
                                        ;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
                                        ;   which can be found in the file epl-v10.html at the root of this distribution.
                                        ;   By using this software in any fashion, you are agreeing to be bound by
                                        ;   the terms of this license.
                                        ;   You must not remove this notice, or any other, from this software.
                                        ;

;;  clojure.test-helper
;;
;;  Utility functions shared by various tests in the Clojure
;;  test suite
;;
;;  tomfaulhaber (gmail)
;;  Created 04 November 2010


;;; A small piece copied from the Clojure test suite

(ns cnrepl.test-helper
  (:use clojure.test))

#?(:clj
   (let [nl (System/getProperty "line.separator")]
     (defn platform-newlines [s] (.replace s "\n" nl)))
   :cljr
   (let [nl Environment/NewLine]
     (defn platform-newlines [s] (.Replace s "\n" nl))))
