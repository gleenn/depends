(ns com.gleenn.depends.visualize
  (:require [com.gleenn.depends.core :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.extra.utils :as utils]
            [clojure2d.color :as c]
            [cljplot.build :as b]
            [cljplot.core :as plot]))

(def mh-instance (metropolis-hastings [0 1 0 1]))

(def samples (->> (nth mh-instance 50000)                   ;; run MCMC inference
                  (:accepted)                               ;; select accepted list
                  (drop 100)                                ;; burn
                  (take-nth 100)                            ;; lag
                  (take 150)))                              ;; samples

(prn (count samples))

(def best (apply max-key log-likelihood samples))

(def img (with-canvas [c (canvas 500 500)]
                      (set-background c :white)
                      (scale c 500)
                      (set-color c 0x99ccff 10)
                      (doseq [[x1 x2 y1 y2] samples]
                        (rect c x1 y1 (- x2 x1) (- y2 y1)))
                      (set-color c (c/darken 0x99ccff))
                      (let [[x1 x2 y1 y2] best]
                        (set-stroke c (/ 500.0))
                        (rect c x1 y1 (- x2 x1) (- y2 y1) true))
                      (set-color c :black)
                      (doseq [{:keys [x y]} data]
                        (ellipse c x y 0.01 0.01))
                      c))

(save img "resources/results/rectangles.jpg")