(ns dwcoin.btest
  (:import [dwcoin.core field-element])
  (:import [dwcoin.core s256-field])
  (:require [dwcoin.core :as bcore]))

(def a 1)

(defn test-fe-pow 
  [ ]
  (let [ a (bcore/field-element. 17 31) 
         b (bcore/field-element. 5 31)
         c (bcore/field-element. 18 31) ]
    (println "Test fe pow")
    (assert (bcore/= (bcore/** a 3) (bcore/field-element. 15 31) ))
    (assert (bcore/= (bcore/* (bcore/** b 5) c) (bcore/field-element. 16 31)))))

(defn test-fe-mul 
 [ ]
  (let [ a (bcore/field-element. 24 31) 
         b (bcore/field-element. 19 31) ]
    (println "Test fe mul")
    (assert (bcore/= (bcore/* a b) (bcore/field-element. 22 31)))))

(defn test-fe-rmul
  [ ]
    (let [ a (bcore/field-element. 24 31) ]
      (assert (= (bcore/*' a 2) (bcore/+ a a)))))

(defn test-s256-new
  [ ]
    (let [ a (bcore/s256-field. 24)
           b (bcore/s256-field. 19) ]
      (println "s256 " + (bcore/* a b))))

(defn test-fe-div
  [ ]
  (let [ a (bcore/field-element. 3 31)
         b (bcore/field-element. 24 31) 
         c (bcore/field-element. 17 31) 
         d (bcore/field-element. 24 31) ]
    (println "test div for fe")
    (assert (= (bcore// a b) (bcore/field-element. 4 31)))
    (assert (bcore/= (bcore/** c -3) (bcore/field-element. 29 31))))
  )

(defn -test
  [& args]
  (bcore/fe-test-0)
  (bcore/point-test-0)
  (bcore/point-test-1)
  (bcore/point-test-2)
  (bcore/fe-test-1)
  (test-fe-div)
  (test-fe-mul)
  (test-fe-rmul)
  (test-fe-pow) 
  (test-s256-new) 
)

