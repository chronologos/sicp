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
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat -2 -14))

(print-rat (add-rat one-half one-half))

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
