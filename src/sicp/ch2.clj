(ns sicp.ch2
  (:import (java.lang Math))
  (:require [clojure.tools.trace :as trace]
            [sicp.ch1]
            [sicp.util]))

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
