(ns dwcoin.core)

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

(defrecord field-element [num prime]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defrecord point [x y a b]
  clojure.lang.IFn
  (invoke [this b] ((keyword b) this)))

(defprotocol field-element-p
  (-mul [a b] "mul with same class or coef")
  (-eq [a b] "compare a and b fe")
  (-minus [x y] "abc"))

(extend-protocol field-element-p
  field-element 
  (-mul [a b]
    (let [ prime (:prime a) 
           self (get-key-or-num a :num)
           factor (get-key-or-num b :num) ]
      (field-element. (mod (* self factor) prime) prime)))

  (-eq [a b] (and (= (a :num) (:num b)) (= (:prime a) (:prime b))))
  (-minus [x y] (str "Minus!!"))
  
  point
  (-eq [a b] (str "point eq"))
  (-minus [x y] (str "point Minus!!")))

(defn fe-eq
  [ a b ]
  (and (= (:num a) (:num b)) (= (:prime a) (:prime b))))

(defn fe-add
  [ a b ]
  (assert (= (:prime a) (:prime b)))
  (field-element. (mod (+ (:num a) (:num b)) (:prime a)) (:prime a)))

(defn pow
  [ x e ]
  (reduce * (repeat e x)))

(defn fe-pow
  [ f e ]
  (field-element. (mod (pow (:num f) (mod e (- (f :prime) 1))) (f :prime)) (f :prime)))

(defn point-eq
  [ a b ]
  (if (or (nil? a) (nil? b)) false
    (and 
      (= (a :x) (b :x))
      (= (a :y) (b :y))
      (= (a :a) (b :a))
      (= (a :b) (b :b)))))

(defn point-add
  [ a b ]
  (assert (and (= (a :a) (b :a)) (= (a :b) (b :b))))
  (if (nil? (a :x)) b
    (if (nil? (b :x)) a
      (if (and (= (a :x) (b :x)) (not (= (a :y) (b :y)))) (point. nil nil (a :a) (a :b)) ; case 1
        (if (not (= (a :x) (b :x)))            
          (let [ s (/ (- (b :y) (a :y)) (- (b :x) (a :x))) 
                 x (- (* s s) (+ (a :x) (b :x)))
                 y (- (* s (- (a :x) x) ) (a :y)) ]
            (point. x y (a :a) (a :b))) 
              (if (and (point-eq a b) (= (a :y) (* 0 (a :x))))
                (point. nil nil (a :a) (a :b))
                  (if (point-eq a b)          
                    (let [ s (/ (+ (a :a) (* 3 (* (a :x) (a :x)))) (* 2 (a :y)) )
                           x (- (* s s) (* 2 (a :x)))
                           y (- (* s (- (a :x) x)) (a :y)) ]
                      (point. x y (a :a) (a :b))) nil)))))))

(defn point-rmul
  [ p coefficeint ]
  (let [ coef (atom coefficeint) 
         result (atom (aclone p))
         current (atom (point. nil nil (p :a) (p :b))) ]

    (while (> @coef 0)
      (if (> (bit-and coef 1) 0)
        (reset! result (point-add result current)) nil)
      (reset! current (point-add current current))
      (reset! coef (/ coef 2)))
  @result))

(defn point-test-0
  [ ]
  (let [ a (point. nil nil 5 7)
         b (point. 2 5 5 7)
         c (point. 2 -5 5 7) ]
    (println "Test for point-0")
    (assert (point-eq (point-add b c) a))
    (assert (point-eq (point-add b a) b))
    (assert (point-eq (point-add a b) b))))

(defn point-test-1
  [ ]
  (let [ a (point. 3 7 5 7)
         b (point. -1 -1 5 7) ]
    (println "Test for point-1")
    (assert (point-eq (point-add a b) (point. 2 -5 5 7)))))

(defn point-test-2
  [ ]
  (let [ a (point. -1 1 5 7) ]
    (println "Test for point-2")
    (assert (point-eq (point-add a a) (point. 18 -77 5 7)))))

(def fe field-element)
(def * -mul)
(def - -minus)
(def = -eq)

(defn fe-test-0
  [ ]
  (let [ f0 (field-element. 17 19)
         f1 (field-element. 8 19) ]
    (println (* f0 f1))
    (assert (= (* f0 f1) (field-element. 3 19)))
    (assert (= (* f0 8) (field-element. 3 19)))))

(defn test
  [ ]
  (let [ e0 (field-element. 3 9)
         e1 (field-element. 2 11)
         e3 (field-element. 11 11)
         e2 (field-element. 3 9) ]
    (println "Test for field-element add, pow operation.")
    (assert (= (field-element. 15 31) (fe-pow (field-element. 17 31) 3 )))
    (assert (= (fe-add e1 e3) e1))
    (assert (not (= e0 e1)))
    (assert (= e0 e2))))

(defn -main
  [& args]
  (println (- (field-element. 1 2) (field-element. 3 4 )))
  (fe-test-0)
  (point-test-0)
  (point-test-1)
  (point-test-2)
  (println (= (point. -1 1 5 6) (point. 1 2 3 4)))
  (test))

