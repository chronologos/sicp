(ns sicp.ch2
  (:import (java.lang Math))
  (:require [clojure.tools.trace :as trace]
            [sicp.ch1]
            [sicp.util]))

;; Chapter 2.1

;; Representing rational numbers
(defn make-rat-v1 [n d]
  (let [g (sicp.ch1/gcd n d)]
    (list (/ n g) (/ d g))))
(defn numer [x] (first x))
(defn denom [x] (second x))
(defn print-rat [x]
  (print (str (numer x) "/" (denom x))))

(defn add-rat [x y]
  (make-rat-v1 (+ (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat-v1 (- (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat-v1 (* (numer x) (numer y))
               (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat-v1 (* (numer x) (denom y))
               (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def some-num (make-rat-v1 -2 -14))

(print-rat (add-rat some-num some-num))

;; Exercise 2.1.  Define a better version of make-rat that handles both positive and negative arguments.
(defn make-rat [n d]
  (let [g (sicp.ch1/gcd n d)
        d-neg (> 0 d)
        n-neg (> 0 n)
        one-neg (and (or d-neg n-neg)
                     (not (and d-neg n-neg)))
        nn (Math/abs n)
        dd (Math/abs d)]
    (list (* (if one-neg -1 1) (/ nn g)) (/ dd g))))

;; Exercise 2.2 line segments in a plane
;; Now, I try it clojure-style :)
(defprotocol Point
  "2D Point"
  (x [this] "Get X coordinate")
  (y [this] "Get Y coordinate")
  (string [this]))

(defprotocol Line
  "2D Point"
  (start [this])
  (end [this])
  (midpoint-segment [this]))

(deftype PointImpl [x y]
  Point
  (x [this] x)
  (y [this] y)
  (string [this] (str x "," y)))

(deftype LineImpl [start end]
  Line
  (start [this] start)
  (end [this] end)
  (midpoint-segment [this]
    (PointImpl. (/ (+ (x start) (x end)) 2) (/ (+ (y start) (y end)) 2))))

(string
 (midpoint-segment (LineImpl. (PointImpl. 0 0) (PointImpl. 2 2))))

;; Skip exercise 2.3

;; Exercise 2.4
;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; (define (cdr z) (z (lambda (p q) q)))
;; (car (cons x y)) = (car (lambda (m) (m x y))) 
;; = ((lambda (m) (m x y)) (lambda (p q) p)))
;; = ((lambda (p q) p)) x y)
;; = x

;; Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair a and b as the integer that is the product 2^a 3^b. Give the corresponding definitions of the procedures cons, car, and cdr.

;; 2 and 3 are prime so th`e prime factorization of the number will be in the form given.

(defn pair-23 [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn factor-degree [n f]
  (let [check (fn [nn i]
                (if (zero? (mod nn f)) (recur (quot nn f) (inc i))
                    i))]
    (check n 0)))

(defn car-23 [n]
  (let [check (fn [nn i]
                (if (zero? (mod nn 2)) (recur (quot nn 2) (inc i))
                    i))]
    (check n 0)))

(defn cdr-23 [n]
  (let [check (fn [nn i]
                (if (zero? (mod nn 3)) (recur (quot nn 3) (inc i))
                    i))]
    (check n 0)))

(car-23 (pair-23 4 5))
(cdr-23 (pair-23 4 5))

;; Exercise 2.6
(defn church-zero [] (fn [f] (fn [x] x)))
(defn church-add1 [n] (fn [f]
                        (fn [x]
                          (f ((n f) x)))))

;; (church-add1 church-zero) =
;;((fn [f1] (fn [x] (f1 ((n f1) x))))
;;   (fn [f2] (fn [x] x)))
;; =>
;;((fn [f1] (fn [x] (f1 (((fn [f2] (fn [x] x)) f1) x))))
;; )
;; =>
;; (fn [f1] (fn [x] (f1 x)))

;; (church-add1 church-one) =
;; ((fn [f1]
;;    (fn [x] (f1
;;             (((fn [f2] (fn [x2] (f2 x2))) f1) x)))))
;; =>
;; ((fn [f1]
;;    (fn [x] (f1
;;             ((fn [x2] (f1 x2)) x)))))
;; =>
;; ((fn [f1]
;;    (fn [x] (f1
;;             (f1 x)))))

;; Eli makes this part look easy
;; https://eli.thegreenplace.net/2007/07/25/sicp-sections-211-212/
;; It took me a while to grok it though. Roughly it corresponds to the following:
;; Unwrap "a" once so it can take in x as argument.
;; Unwrap "b" twice so it becomes a bunch of composed function calls.
;; call a on b so that the calls compose.
(defn church-add-a-b [a b]
  (fn [f]
    (fn [x]
      (((a f) ((b f) x))))))

;; Exercise 2.7
(defn make-interval [upper lower] [upper lower])

(defn upper-bound [i] (first i))
(defn lower-bound [i] (second  i))

;; Exercise 2.8 - computing the difference of two intervals

(defn sub-interval [a b]
  (let [new-upper-bound (- (upper-bound a) (lower-bound b))
        new-lower-bound (- (lower-bound a) (upper-bound b))]
    (make-interval new-upper-bound new-lower-bound)))

(sub-interval (make-interval 20 10) (make-interval 2 1))

(defn mul-interval [a b]
  (let [p1 (* (lower-bound a) (lower-bound b))
        p2 (* (lower-bound a) (upper-bound b))
        p3 (* (upper-bound a) (lower-bound b))
        p4 (* (upper-bound a) (upper-bound b))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;; Exercise 2.9.  The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.

;; in the cae of addition, instead of representing an interval by the upper and lower bound, we could represent it by the lower bound and the width. Then we would have

(defn add-interval [[lb-a width-a] [lb-b width-b]]
  [(+ lb-a lb-b) (+ width-a width-b)])

(add-interval [1 2] [1 3])

;; The same is true for subtraction but not for multiplication and division. To see the latter, look at the formula given by Gerald and Sussman for mul-interval again: terms p2, p3 and p4 involve upper bounds, which means that in the language of the alternative representation ([lb width]) given above, widths are being multiplied with lower bounds and subsequently used to determine the widths. The same argument applies for division.

;; Exercise 2.10
(defn div-interval [a b]
  (let [lb-b (lower-bound b)
        ub-b (upper-bound b)]
    (if (< (* ub-b lb-b) 0)
      (throw (new AssertionError "cannot divide by span over zero"))
      (mul-interval a
                    (make-interval (/ 1.0 (upper-bound b))
                                   (/ 1.0 (lower-bound b)))))))

;; Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.'' Rewrite this procedure using Ben's suggestion.
;; TODO SKIP, see https://eli.thegreenplace.net/2007/07/27/sicp-section-214/

;; Exercise 2.12
(defn make-center-percent [c tol]
  (let [abs (* c (/ tol 100))]
    (make-interval (- c abs) (+ c abs))))

(make-center-percent 10 100)

;; Exercise 2.13
;; For proof, see SICP notability notebook.

;; Exercise 2.14
(div-interval (make-center-percent 5 1) (make-center-percent 5 1))
(div-interval (make-center-percent 5 1) (make-center-percent 10 1))
;; Exercise 2.15 & Exercise 2.16
;; From wikipedia:
;; > Informally, a field is a set, along with two operations defined on that set: an addition operation written as a + b, and a multiplication operation written as a ⋅ b, both of which behave similarly as they behave for rational numbers and real numbers, including the existence of an additive inverse −a for all elements a, and of a multiplicative inverse b−1 for every nonzero element b. This allows one to also consider the so-called inverse operations of subtraction, a − b, and division, a / b, by defining:
;; In intervals there is no multiplicative inverse!

;; Chapter 2.2
;; Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list.
;; It would be trivial using a vector in Clojure, but let's try what the book wants.
(defn last-elem [l]
  (if (empty? (rest l)) (first l)
      (last-elem (rest l))))

(last-elem '(1 2 3))

;; Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:
(reverse '(1 2 3)) ; okay not this :)

(defn reverse-list
  ([l res] (if (empty? l) res
               (reverse-list (rest l) (cons (first l) res))))
  ([l] (reverse-list l '())))

(reverse-list '(1 2 3))

;; Exercise 2.19
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn first-denomination [d] (first d))
(defn except-first-denomination [d] (rest d))

(defn no-more? [d] (empty? d))

(defn cc [amount coin-values]
  (cond (zero? amount) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values))))
(cc 100 us-coins)

;; Exercise 2.20
;; In Clojure, we use (defn f [a & args]) to have variadic arguments
;; I was having a little fun with sequence destructuring here; it would probably be nicer to do some of the destructuring as code.
(defn same-parity [[x & [_ & rest-xs :as xs] :as all]]
  (if (or (empty? all) (empty? xs)) '()
      (cons x (same-parity rest-xs))))

(same-parity (range 1 11))

(assert (= (same-parity (range 1 11)) '(1 3 5 7 9)))

;; Exercise 2.21

(defn square-list-1 [items]
  (if (empty? items) nil
      (cons (sicp.util/square (first items)) (square-list-1 (rest items)))))
(square-list-1 [1 2 3])

(defn square-list-2 [items]
  (map sicp.util/square items))
(square-list-2 '(1 2 3))

;; Exercise 2.22
;; In the first implementation, list elements are taken out of `things` from the front of the list and added onto the front of the result, causing the result to be reversed.

;; In the second implementation, the returned list isn't flat. In fact, a straightforward translation into clojure doesn't work because the params of cons are [x seq] but we are trying to pass a seq as the first argument. A revised version that works:
(defn square-list [items]
  (let [iter (fn [things answer]
               (if (empty? things) answer
                   (recur (rest things)
                          (concat answer (list (sicp.util/square (first things)))))))]
    (iter items '())))

(square-list '(1 2 3 4))

;; Exercise 2.23

(defn for-each [f s]
  (reduce (fn [_ val] (f val)) nil s))
(for-each #(println %) '(1 2 3))

;; alternatively, since clojure uses lazy seqs, we can force evaluation using doall.
(doall (map #(println %) '(1 2 3)))

;; 2.6
(defn count-leaves [x]
  (cond (not (seq? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(count-leaves '(1 (2 (3 4))))

;; Exercise 2.24 in notability
;; Exercise 2.25
;; So Clojure has sensibly renamed car and cdr for modern sensibilities. 
;; first = car, rest = cdr
(defn cadr [x] (first (rest x)))
((comp cadr cadr rest) '(1 3 (5 7) 9))
((comp first first) '((7)))
((comp cadr cadr cadr cadr cadr cadr) '(1 (2 (3 (4 (5 (6 7)))))))

;; Exercise 2.26
(def list-x (list 1 2 3))
(def list-y (list 4 5 6))
(concat list-x list-y) ;; i think append is concat in clojure?, it gives (1 2 3 4 5 6)
(cons list-x list-y) ;; gives ((1 2 3) 4 5 6)
(list list-x list-y) ;; gives ((1 2 3) (4 5 6))

;; Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,

;; Similar to count-leaves
(defn deep-reverse [x]
  (cond (not (seq? x)) x ;; if not a seq (e.g. nil or atom) just return the element on its own.
        :else (reverse (map deep-reverse x)))) ;; recurse
(deep-reverse '((1 2) (3 4) ()))

;; Exercise 2.28
;; Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. 
(def fringe-testinput (list (list 1 2) (list 3 4)))

;; I think this is the idiomatic way :) ; super easy due to tree-seq.
;; Picked it up when I was doing 4Clojure exercises.
(defn fringe [l]
  (filter #(not (seq? %)) (tree-seq seq? identity l)))
(fringe (list fringe-testinput fringe-testinput))

(defn fringe-no-cheat [x]
  (cond
    (not (seq? x)) (list x) ;; if it is a leaf, return wrapped in a list for `concat`
    (empty? x) nil ;; if it is empty or nil, return nil
    :else (concat (fringe-no-cheat (first x))
                  (fringe-no-cheat (rest x)))))

(fringe-no-cheat fringe-testinput)
(fringe-no-cheat (list fringe-testinput fringe-testinput))

;; TODO I did 2.29 to 2.32 but didn't save them properly...

;; Exercise 2.33.  Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(defn accumulate [op initial sequence]
  (let [next (fn next [op initial sequence]
               (if (empty? sequence)
                 initial
                 (op (first sequence)
                     (next op initial (rest sequence)))))]
    (next op initial sequence)))

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))

(defn map_ [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) nil sequence))

(map_ #(* % 2) '(1 2 3))

(defn append_ [seq1 seq2]
  (accumulate cons seq2 seq1))

(append_ '(1 2 3) '(4 5 6))

(defn length_ [sequence]
  (accumulate (fn [x y] (if (zero? y) 1 (inc y))) 0 sequence))
;;(length_ '(1 2 3))

;; Exercise 2.34
(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an accumulation:
(defn count-leaves_ [t]
  (accumulate
   (fn [this-form num-leaves]
     (if (not (seq? this-form)) (inc num-leaves)
         (+ num-leaves (count-leaves this-form))))
   0 t))

;;(count-leaves_ '(1 (2 3 (3 4))))

;; Exercise 2.36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

;;(accumulate-n + 0 (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))

;; Exercise 2.37s
(def matrix-a (list '(1 2 3 4) '(4 5 6 6) '(6 7 8 9)))
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(4 5 6))

(defn matrix-*-vector [m v] (map #(dot-product v %) m))
;;(matrix-*-vector matrix-a '(1 1 2 1))

(defn transpose [m] (accumulate-n cons nil m))
;;(transpose matrix-a)

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

;;(matrix-*-matrix (list '(1 2 3) '(4 5 6)) (list '(7 8) '(9 10) '(11 12)))

;; Exercise 2.38
(defn foldr [op init seq]
  (if (empty? seq) init
      (op (first seq)
          (foldr op init (rest seq)))))

(defn foldl [op init seq]
  (let [iter (fn [result l]
               (if (empty? l) result
                   (recur (op result (first l)) (rest l))))]
    (iter init seq)))

(foldr / 1 '(1 2 3)) ;; 3/2
(foldl / 1 '(1 2 3)) ;; 1/6
;;(foldr list nil (list 1 2 3)) ;; (1 (2 (3 nil)))
;;(foldl list nil (list 1 2 3)) ;; (((nil 1) 2) 3)

;; Exercise 2.39
(defn reverse-r [sequence]
  (foldr (fn [x y] (concat y (list x))) nil sequence))
;;(reverse-r '(1 2 3))

(defn reverse-l [sequence]
  (foldl (fn [x y] (cons y x)) nil sequence))
;;(reverse-l '(1 2 3))

;; Exercise 2.40
(defn unique-pairs [n]
  (mapcat
   (fn [i] (map
            (fn [j] (list j i))
            (range 1 i)))
   (range 1 (inc n))))
;;(unique-pairs 5)


(defn prime-sum? [pair]
  (sicp.ch1/prime? (+ (first pair) (second pair))))

(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (->> (unique-pairs n)
       (filter prime-sum?)
       (map make-pair-sum)))

;;(prime-sum-pairs 6)

;; Exercise 2.41
(defn unique-triples [n]
  (mapcat
   (fn [i] (mapcat
            (fn [j] (map (fn [k] (list i j k))
                         (range 1 j)))
            (range 1 i)))
   (range 1 (inc n))))
(unique-triples 4)

(defn three-sum [n s]
  (->> (unique-triples n)
       (filter #(= s (apply + %)))))

;;(three-sum 10 10)

;; row position of each crab, in column order.
(def empty-board [])
(defn safe? [k-col-pos positions]
  ;; maybe off by one?
  (let [k-row-pos (nth positions k-col-pos)
        exclude-last (subvec positions 0 k-col-pos)
        ;; compute diagonal 1
        tlbr (mapv - (range) exclude-last)
        ;; compute diagonal 2
        bltr (mapv + (range) exclude-last)]
    (and (not (some #{k-row-pos} exclude-last))
         (not (some #{(- k-col-pos k-row-pos)} tlbr))
         (not (some #{(+ k-col-pos k-row-pos)} bltr)))))
(defn adjoin-position [new-row _ rest-of-queens] (conj rest-of-queens new-row))
;;(safe? 3 [0 0 0 4])

;; Excercise 2.42
(defn queens [n]
  (letfn [(queens-cols [k]
            (if (zero? k)
              (list empty-board)
              (filter (fn [positions] (safe? (dec k) positions))
                      (mapcat
                       (fn [rest-of-queens]
                         (map
                          #(adjoin-position % k rest-of-queens)
                          (range n)))
                       (queens-cols (dec k))))))]
    (queens-cols n)))
;;(count (queens 8))

;; Exercises as part of <A Picture Language> are in ch2quil.clj

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(memq 'apple '(x (apple sauce) y apple pear))

;; Exercise 2.53.  What would the interpreter print in response to evaluating each of the following expressions?

;; (list 'a 'b 'c)
;; (a b c)

;; (list (list 'george))
;; ((george))

;; (rest '((x1 x2) (y1 y2)))
;; ((y1 y2))

;; (first (rest '((x1 x2) (y1 y2))))
;; (y1 y2)

;; (memq 'red '((red shoes) (blue socks)))
;; false

;; (memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; Exercise 2.54.  Two lists are said to be equal? if they contain equal elements arranged in the same order. For example,
;; (= '(a b c) '(a b c))

(defn equal? [a b]
  (cond
    (and (symbol? a) (symbol? b) (= a b)) true
    (and (empty? a) (empty? b)) true
    (and (list? a)
         (list? b)
         (equal? (first a) (first b))
         (equal? (rest a) (rest b))) true
    :else false))

(equal? '(a b c) '(a b c))
(first ''abracadabra)

(defn variable? [x] (symbol? x))
(defn same-variable? [x y] (and (variable? x) (variable? y) (= x y)))
(defn sum? [x] (and (list? x) (= (first x) '+)))
(defn make-sum [x y]
  (cond (and (number? x) (number? y)) (+ x y)
        (and (number? x) (zero? x)) y
        (and (number? y) (zero? y)) x
        :else (list '+ x y)))
(defn addend [x] (second x))
(defn augend [x]
  (if (= (count x) 3) (nth x 2)
      (conj (rest (rest x)) '+)))
(defn product? [x] (and (list? x) (= (first x) '*)))
(defn make-product
  ([x y]
   (cond (and (number? x) (number? y)) (* x y)
         (and (number? x) (zero? x)) 0
         (and (number? y) (zero? y)) 0
         (= x 1) y
         (= y 1) x
         :else (list '* x y)))
  ([x y & args] #p (apply make-product (make-product x y) args)))
(defn multiplier [x] (second x))
(defn multiplicand [x]
  (if (= (count x) 3) (nth x 2)
      (conj (rest (rest x)) '*)))
(defn exponentiation? [x] (and (list? x) (= (first x) '**)))
(defn base [x] (second x))
(defn exponent [x] (nth x 2))
(defn make-exponentiation [x y]
  (cond (and (number? y) (zero? y)) 1
        (and (number? y) (= y 1)) x
        :else (list '** x y)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum
                    #p (deriv (addend exp) var)
                    #p (deriv (augend exp) var))
        (product? exp)
        (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                  (make-product (multiplicand exp) (deriv (multiplier exp) var)))
        (exponentiation? exp)
        (make-product #p (exponent exp)
                      #p (make-exponentiation (base exp) (dec (exponent exp)))
                      #p (deriv #p (base exp) var))
        :else (throw (new Exception))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(* x (** x 3)) 'x)

;; Ex 2.58b, adapted from https://eli.thegreenplace.net/2007/08/30/sicp-sections-231-232
;;
;; <sum>    -> <term> + <sum>
;;           | <term>
;;
;; <term>   -> <factor> * <term>
;;           | <factor>
;;
;; <factor> -> symbol 
;;           | number
;;           | (<sum>)
;;

(def token-stream (atom '()))
(defn initialize-token-stream [s] (swap! token-stream (fn [x] s)))
(def stream-stack (atom '()))
(defn push-stream [stream]
  (swap! stream-stack conj @token-stream)
  (initialize-token-stream stream))
(defn pop-stream [] (let [top (swap! stream-stack pop)]
                      (initialize-token-stream top)))
(defn next-token [] (first @token-stream))
(defn scan [] (let [r (next-token)]
                (swap! token-stream rest)
                r))

(declare parse-factor)

(defn parse-term []
  (let [lfact (parse-factor)]
    (if (= (next-token) '*)
      (do (scan)
          (let [rterm (parse-term)]
            (list '* lfact rterm)))
      lfact)))

(defn parse-sum []
  (let [lterm (parse-term)]
    (if (= (next-token) '+)
      (do (scan)
          (let [rsum (parse-sum)]
            (list '+ lterm rsum)))

      lterm)))

(defn parse-factor []
  (let [tok (next-token)]
    (cond (or (number? tok) (symbol? tok)) (do (scan) tok)
          (list? tok) (do (push-stream tok) (let [sum (parse-sum)]
                                              (pop-stream)
                                              (scan)
                                              sum))
          :else (throw (new Exception)))))


;; run with:
;; (initialize-token-stream '(x + 3 * (x + y + 2)))
;; (parse-sum)
