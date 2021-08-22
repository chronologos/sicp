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

;; segment is a pair of vectors.
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

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter (make-frame new-origin (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

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
  (let [main-frame (make-frame (make-vect 300 300) (make-vect 0 -100) (make-vect -100 0))
        main-frame-flipped (make-frame (make-vect 300 300) (make-vect 100 0) (make-vect 0 100))]
    (q/resize-sketch 400 400)
    (q/stroke 255 0 0)
    (q/stroke-weight 2)
    (frame-outline main-frame)
    (q/stroke 0 255 0)
    (frame-outline (make-frame (make-vect 100 100) (make-vect -50 0) (make-vect 0 -50)))
    ((segments->painter [[[0 0] [1 1]]])
     (make-frame (make-vect 100 100) (make-vect -50 0) (make-vect 0 -50)))
    (q/stroke 0 0 255)
    (frame-x main-frame)
    (q/stroke 122 122 122)
    (frame-diamond main-frame)
    (q/stroke 122 122 0)
    (base-image main-frame)
    (q/stroke 0 122 122)
    ((flip-vert base-image) main-frame)))


(q/defsketch example                  ;; Define a new sketch named example
  :title "lines"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup setup                        ;; Specify the setup fn
  :draw draw                          ;; Specify the draw fn
  :size [323 200])                    ;; You struggle to beat the golden ratio

(defn -main [& args])
