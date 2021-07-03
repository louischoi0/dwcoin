(ns dwcoin.core
    (:require [clojure.math.numeric-tower :as math]))

(require '[byte-streams :as c])

(defn get-key-or-num
  [object k]
  (if (instance? Number object) 
    object
    ((keyword k) object)))

(defn .contains?
  [o k]
  (try 
    (contains? o k)
  (catch Exception e false)))

(defn bytes-to-int
  ([bytes]
          (bytes-to-int bytes 0))
  ([bytes offset]
    (reduce + 0
      (map (fn [i]
        (let [shift (* (- 4 1 i) 8)]
          (bit-shift-left (bit-and (nth bytes (+ i offset)) 0x000000FF) shift))) (range 0 4)))))

(defn bytes->num 
    [data]
      (reduce bit-or (map-indexed (fn [i x] (bit-shift-left (bit-and x 0x0FF) (* 8 (- (count data) i 1)))) data)))

(defn sec
  [ s256p c ]
    (let [ y-num (:num s256p) 
           x-num (:num s256p) ]))


(defrecord field-element [num prime]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defrecord point [x y a b]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defprotocol field-element-p
  (-mul [a b] "mul with same class or coef")
  (-rmul [a m] "mul with scalar")
  (-eq [a b] "compare a and b fe")
  (-pow [a e] "exponenetail")
  (-add [a b] "add two class")
  (-div [a b] "div two fe")
  (-sub [a b] "sub two class")
  (-sqrt [a] "sqrt"))


(defn pow
  [ x e ]
  (Math/pow x e))

(def A 0)
(def B 7)
(def P (- (math/expt 2 256) (math/expt 2 32) 977))
(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)
(def Pd4 28948022309329048855892746252171976963317496166410141009864396001977208667916)

(defn big-or
    [f & r]
      (reduce (fn [acc v] (.or acc (biginteger v))) (biginteger f) r))

(defn big-and
    [f & r]
      (reduce (fn [acc v] (.and acc (biginteger v))) (biginteger f) r))

(defn big-xor
    [f & r]
      (reduce (fn [acc v] (.xor acc (biginteger v))) (biginteger f) r))

(defrecord s256-field 
  [ num ]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defrecord s256-point [x y]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defprotocol point-op-p
  (-sec [ p compressed ] "return the binary version of the SEC format")
  (-verify [ a z sig ] "verify s256"))

(defn s256-point-cv
  [ a ]
    (point. (a :x) (a :y) (s256-field. A) (s256-field. B)))

(defrecord signature
  [ r s ]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defn mpow "modular exponentiation" [b e m]
    (defn m* [p q] (mod (* p q) m))
      (loop [b b, e e, x 1]
        (if (zero? e) x
          (if (even? e) (recur (m* b b) (/ e 2) x)
            (recur (m* b b) (quot e 2) (m* b x))))))

(defn zfill
  [ s len ]
    (let [ addlen (- len (count s)) ]
      (str (apply str (repeat addlen "0")) s)))

(defn hexify
  [ x len ]
    (zfill (.toString (biginteger x) 16) len))

(extend-protocol point-op-p
  s256-field
    (-sec
      [ p compressed ]
        (if compressed
          (if (zero? (mod (p :y) 2))
            (str "02" (hexify (:num (p :x))))
            (str "03" (hexify (:num (p :x)))))
          (str "04" (hexify (:num (p :x))))))

  s256-field
    (-verify
      [ a z sig ] 1))

(extend-protocol field-element-p
  field-element 
  (-mul [a b]
    (let [ prime (:prime a) 
           self (get-key-or-num a :num)
           factor (get-key-or-num b :num) ]
      (field-element. (mod (* self factor) prime) prime)))

  (-rmul 
    [a x]
    (let [ num (mod (* (a :num) x) (a :prime) ) ]
      (field-element. num (a :prime))))

  (-eq [a b] (and (= (a :num) (:num b)) (= (:prime a) (:prime b))))

  (-pow
    [ a e ]
    (let [ n (mod e (- (a :prime) 1)) 
           num (mpow (a :num) n (a :prime)) ] 
      (field-element. num (a :prime))))

  (-add
    [ a b ]
    (field-element. (mod (+ (:num a) (:num b)) (:prime a)) (:prime a)))

  (-div
    [ a b ]
    (let [nnum (mod (-mul (:num a) (mpow (b :num) (- (a :prime) 2), (a :prime) )) (a :prime)) ]
      (field-element. nnum (a :prime))))
  
  s256-field
  (-mul [a b]
    (let [a (field-element. (a :num) P)
          b (field-element. (b :num) P) ]
      (-mul a b)))
  
  (-rmul 
    [a x]
    (let [ a (field-element. (a :num) P)
           num (mod (* (a :num) x) (a :prime) ) ]
      (field-element. num (a :prime))))

  (-eq [a b]
    (let [a (field-element. (a :num) P)
          b (field-element. (b :num) P) ]
      (-eq a b)))

  (-pow
    [ a e ]
    (let [ fa (field-element. (a :num) P)
           n (mod e (- (fa :prime) 1)) 
           num (mpow (a :num) n (fa :prime)) ] 
      (field-element. num (fa :prime))))

  (-add
    [ a b ]
    (let [ a (field-element. (a :num) P)
          b (field-element. (b :num) P) ]
      (field-element. (mod (+ (:num a) (:num b)) (:prime a)) (:prime a))))

  (-div
    [ a b ]
    (let [ a (field-element. (a :num) P)
           b (field-element. (b :num) P) 
           nnum (mod (-mul (:num a) (mpow (b :num) (- (a :prime) 2), (a :prime) )) (a :prime)) ]
      (field-element. nnum (a :prime))))

  (-sqrt 
    [ a ]
      (let [ a (field-element. (a :num) P) ]
        (-pow a Pd4)))

  java.lang.Long
  (-mul [a b]
    (clojure.core/* a b ))

  (-div [a b]
    (clojure.core// a b ))
  
  point
  (-eq [ a b ]
    (if (or (nil? a) (nil? b)) false
      (and 
        (= (a :x) (b :x))
        (= (a :y) (b :y))
        (= (a :a) (b :a))
        (= (a :b) (b :b)))))

  (-add [ a b ]
    (assert (and (= (a :a) (b :a)) (= (a :b) (b :b))))
    (if (nil? (a :x)) b
      (if (nil? (b :x)) a
        (if (and (= (a :x) (b :x)) (not (= (a :y) (b :y)))) (point. nil nil (a :a) (a :b)) ; case 1
          (if (not (= (a :x) (b :x)))            
            (let [ s (/ (- (b :y) (a :y)) (- (b :x) (a :x))) 
                   x (- (* s s) (+ (a :x) (b :x)))
                   y (- (* s (- (a :x) x) ) (a :y)) ]
              (point. x y (a :a) (a :b))) 
                (if (and (= a b) (= (a :y) (* 0 (a :x))))
                  (point. nil nil (a :a) (a :b))
                    (if (= a b)          
                      (let [ s (/ (+ (a :a) (* 3 (* (a :x) (a :x)))) (* 2 (a :y)) )
                             x (- (* s s) (* 2 (a :x)))
                             y (- (* s (- (a :x) x)) (a :y)) ]
                        (point. x y (a :a) (a :b))) nil)))))))
  (-mul
    [ p coefficeint ]
    (let [ coef (atom coefficeint) 
           result (atom (aclone p))
           current (atom (point. nil nil (p :a) (p :b))) ]

      (while (> @coef 0)
        (if (> (bit-and coef 1) 0)
          (reset! result (+ result current)) nil)
        (reset! current (+ current current))
        (reset! coef (/ coef 2)))
    @result))
  
  s256-point
  (-eq 
    [a b]
      (let [ a (s256-point-cv a)
             b (s256-point-cv b) ]
        (-eq a b)))

  (-rmul
    [ a x ]
      (let [ coef (mod x N) ] 
        (-rmul (s256-point-cv a) coef)))

  (-add 
    [ a b ]
        (let [ a (s256-point-cv a)
               b (s256-point-cv b) ]
          (-add a b)))

  (-mul
    [ a b ]
        (let [ a (s256-point-cv a)
               b (s256-point-cv b) ]
          (-mul a b)))

)

(def fe field-element)
(def * -mul)
(def *' -rmul)
(def = -eq)
(def + -add)
(def ** -pow)
(def **. -sqrt)
(def / -div)

(defn point-test-0
  [ ]
  (let [ a (point. nil nil 5 7)
         b (point. 2 5 5 7)
         c (point. 2 -5 5 7) ]
    (println "Test for point-0")
    (assert (= (+ b c) a))
    (assert (= (+ b a) b))
    (assert (= (+ a b) b))))

(defn point-test-1
  [ ]
  (let [ a (point. 3 7 5 7)
         b (point. -1 -1 5 7) ]
    (println "Test for point-1")
    (assert (= (+ a b) (point. 2 -5 5 7)))))

(defn point-test-2
  [ ]
  (let [ a (point. -1 1 5 7) ]
    (println "Test for point-2")
    (assert (= (+ a a) (point. 18 -77 5 7)))))

(defn fe-test-0
  [ ]
  (let [ f0 (field-element. 17 19)
         f1 (field-element. 8 19) ]
    (println (* f0 f1))
    (assert (= (* f0 f1) (field-element. 3 19)))
    (assert (= (* f0 8) (field-element. 3 19)))))

(defn fe-test-1
  [ ]
  (let [ e0 (field-element. 3 9)
         e1 (field-element. 2 11)
         e3 (field-element. 11 11)
         e2 (field-element. 3 9) ]
    (println "Test for field-element add, pow operation.")
    (assert (= (field-element. 15 31) (-pow (field-element. 17 31) 3 )))
    (assert (= (+ e1 e3) e1))
    (assert (not (= e0 e1)))
    (assert (= e0 e2))))


(load "test")

(defn -main
  [& args]
  (dwcoin.btest/-test)
  (fe-test-0)
  (point-test-0)
  (point-test-1)
  (point-test-2)
  (println (= (point. -1 1 5 6) (point. 1 2 3 4))))

