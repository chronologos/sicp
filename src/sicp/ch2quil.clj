(ns sicp.ch2quil
  (:import (java.lang Math))
  (:require [clojure.tools.trace :as trace]
            [sicp.ch1]
            [sicp.util]
            [quil.core :as q]))

;; 2.44+ using quil library for drawing.
;; need to use java 8... do the following to run:
;; export JAVA_HOME=`/usr/libexec/java_home -v 1.8`; java -version
;; lein run
;; https://stackoverflow.com/questions/21964709/how-to-set-or-change-the-default-java-jdk-version-on-macos

;; This section of the book takes a top-down approach to explaining the problem, but the solution is of course bottom-up. So later exercises in the chapter may come earlier in the code.

;; Ex 2.46
(defn make-vect [x y]
  [x y])
(defn xcor-vect [v] (first v))
(defn ycor-vect [v] (second v))
(defn add-vect
  ([v1 v2] (make-vect
            (+ (xcor-vect v1) (xcor-vect v2))
            (+ (ycor-vect v1) (ycor-vect v2))))
  ([v1 v2 & vs] (reduce add-vect (conj vs v1 v2))))
(defn sub-vect [v1 v2] (add-vect v1
                                 (make-vect (- (xcor-vect v2))
                                            (- (ycor-vect v2)))))
(defn scale-vect [s v] (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; Ex 2.47
;; frame is described by 3 vectors
(defn make-frame [origin edge1 edge2]
  [origin edge1 edge2])
(defn origin-frame [f] (first f))
(defn edge1-frame [f] (second f))
(defn edge2-frame [f] (nth f 2))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect (origin-frame frame)
              (scale-vect (xcor-vect v) (edge1-frame frame))
              (scale-vect (ycor-vect v) (edge2-frame frame)))))


;; Ex 2.48 NOTE: I got lazy and just used a 2 element vector directly.
;; So a segment is a pair of vectors.
;; A Painter takes in a frame and prints out stuff.
(defn segments->painter [segment-list]
  (fn [frame]
    (doall (map (fn [segment] (q/line ((frame-coord-map frame)
                                       (first segment))
                                      ((frame-coord-map frame)
                                       (second segment))))
                segment-list))))

(defn frame-outline [frame]
  ((segments->painter [[(make-vect 0 0) (make-vect 0 1)]
                       [(make-vect 0 0) (make-vect 1 0)]]) frame))

(defn frame-x [frame]
  ((segments->painter [[(make-vect 1 0) (make-vect 0 1)]
                       [(make-vect 0 0) (make-vect 1 1)]]) frame))

(defn frame-diamond [frame]
  ((segments->painter [[(make-vect 0 0.5) (make-vect 0.5 0)]
                       [(make-vect 0.5 0) (make-vect 1 0.5)]
                       [(make-vect 1 0.5) (make-vect 0.5 1)]
                       [(make-vect 0.5 1) (make-vect 0 0.5)]]) frame))

;; Painter operations are based on the procedure transform-painter, which takes as arguments a painter and information on how to transform a frame and produces a new painter. The transformed painter, when called on a frame, transforms the frame and calls the original painter on the transformed frame. The arguments to transform-painter are points (represented as vectors) that specify the corners of the new frame: When mapped into the frame, the first point specifies the new frame's origin and the other two specify the ends of its edge vectors. Thus, arguments within the unit square specify a frame contained within the original frame.
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame #p new-origin
                           #p (sub-vect (m corner1) new-origin)
                           #p (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(defn flip-horiz [painter]
  (transform-painter painter (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;; Ex 2.50

;; Ex 2.51
(defn below [painter1 painter2]
  (let [split-point (make-vect 0 0.5)
        paint-down (transform-painter painter1 (make-vect 0 0) (make-vect 1 0) split-point)
        paint-up (transform-painter painter2 split-point (make-vect 1 0.5) (make-vect 0 1))]
    (fn [frame]
      (paint-down frame)
      (paint-up frame))))

(defn beside [left-painter right-painter]
  (let [split-point (make-vect 0.5 0)
        paint-left (transform-painter left-painter (make-vect 0 0) split-point (make-vect 0 1))
        paint-right (transform-painter right-painter split-point (make-vect 1 0) (make-vect 0.5 1))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

;; ex 2.44
(defn right-split-old [painter n]
  (if (zero? n) painter
      (let [smaller (right-split-old painter (dec n))]
        (beside painter (below smaller smaller)))))

(defn up-split-old [painter n]
  (if (zero? n) painter
      (let [smaller (up-split-old painter (dec n))]
        (below painter (beside smaller smaller)))))

;; ex 2.45 - define right and up split as general splitting operationa
(defn split* [op1 op2]
  (fn split-x [painter n]
    (if (zero? n) painter
        (let [smaller (split-x painter (dec n))]
          (op1 painter (op2 smaller smaller))))))

(def right-split (split* beside below))
(def up-split (split* below beside))

(defn corner-split [painter n]
  (if (zero? n) painter
      (let [up (up-split painter (dec n))
            right (right-split painter (dec n))
            top-left (beside up up)
            bottom-right (below right right)
            corner (corner-split painter (dec n))]
        (beside (below painter top-left) (below bottom-right corner)))))

(defn square-limit [painter n]
  (let [quarter (corner-split painter n)
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))

;; drawing code
(defn setup []
  (q/frame-rate 1)
  (q/background 200))

(defn base-image [frame]
  ((segments->painter [[(make-vect 0 0.25) (make-vect 0.5 0)]
                       [(make-vect 0.5 0) (make-vect 0.75 0.5)]
                       [(make-vect 0.9 0.5) (make-vect 0.55 0.9)]
                       [(make-vect 0.15 0.35) (make-vect 0.15 0.55)]]) frame))

(defn draw []
  (let [main-frame (make-frame (make-vect 300 300) (make-vect 100 0) (make-vect 0 -100))
        main-frame-high (make-frame (make-vect 100 100) (make-vect 100 0) (make-vect 0 -100))
        main-frame-low (make-frame (make-vect 500 500) (make-vect 100 0) (make-vect 0 -100))]
    (q/resize-sketch 600 600)
    (q/stroke 255 0 0)
    (q/stroke-weight 2)
    (frame-outline main-frame)
    ;; (frame-outline (make-frame (make-vect 100 100) (make-vect -50 0) (make-vect 0 -50)))
    ;; ((segments->painter [[[0 0] [1 1]]])
    ;;  (make-frame (make-vect 100 100) (make-vect -50 0) (make-vect 0 -50)))
    (q/stroke 0 0 255)
    (frame-x main-frame)
    (q/stroke 122 122 122)
    (frame-diamond main-frame)
    (q/stroke 0 255 0)
    ((right-split base-image 2) main-frame)
    ;; ((beside base-image base-image) main-frame)
    (q/stroke 0 122 122)
    (base-image main-frame-high)
    (q/stroke 122 122 0)
    ((below base-image base-image) main-frame-low)))


(q/defsketch example                  ;; Define a new sketch named example
  :title "lines"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [323 200])                    ;; You struggle to beat the golden ratio

(defn -main [& args])
