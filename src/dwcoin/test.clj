(ns dwcoin.btest
  (:require [dwcoin.core :as bcore]))

(def a 1)


(defn -test
  [& args]
  (bcore/fe-test-0)
  (bcore/point-test-0)
  (bcore/point-test-1)
  (bcore/point-test-2)
  (bcore/fe-test-1))

(defn test-fe-div
  [ ]
  (let [ a (bcore/field-element. 3 31)
         b (bcore/field-element. 24 31) ]))







