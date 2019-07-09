(ns com.gleenn.depends.core
  (:require [fastmath.core :as m]
            [fastmath.random :as r]))

(def observe r/log-likelihood)

;; 4 data points
(def data [{:x 0.40 :y 0.70}
           {:x 0.50 :y 0.40}
           {:x 0.46 :y 0.63}
           {:x 0.43 :y 0.51}])

(def priors (repeat 4 (r/distribution :uniform-real {:lower 0 :upper 1})))

(let [p1 (first priors)]
  {:sample         (r/sample p1)
   :pdf            (r/pdf p1 0.5)
   :log-pdf        (r/lpdf p1 0.5)
   :mean           (r/mean p1)
   :likelihood     (r/likelihood p1 [0 0.5 1])
   :log-likelihood (observe p1 [0 0.5 1])})

(defn model
  [[x1 x2 y1 y2]]
  (if (and (< x1 x2)
           (< y1 y2))                                       ;; ranges validation
    (let [distr-x (r/distribution :uniform-real {:lower x1 :upper x2})
          distr-y (r/distribution :uniform-real {:lower y1 :upper y2})]
      (+ (observe distr-x (mapv :x data))
         (observe distr-y (mapv :y data))))                 ;; mapData in the book
    ##-Inf))

(model [0.1 0.2 0.3 0.4])

(defn model
  [[x1 x2 y1 y2]]
  (if (and (< x1 x2)
           (< y1 y2))                                       ;; ranges validation
    (let [distr-x (r/distribution :uniform-real {:lower x1 :upper x2})
          distr-y (r/distribution :uniform-real {:lower y1 :upper y2})]
      (+ (observe distr-x (mapv :x data))
         (observe distr-y (mapv :y data))))                 ;; mapData in the book
    ##-Inf))

;; log likelihood of some rectangles
(prn {:valid-rectangle          (model [0.0 1.0 0.1 0.7])
      :invalid-rectangle        (model [-1.0 1.0 0.3 1.3])
      :missing-points-rectangle (model [0.1 0.2 0.1 0.3])})

(defn priors-log-likelihood
  [current]
  (reduce + (map #(r/lpdf %1 %2) priors current)))

(prn {:valid-rectangle   (priors-log-likelihood [0.5 0.5 0.5 0.5])
      :invalid-rectangle (priors-log-likelihood [0.5 -0.5 0.5 0.5])})


(defn log-likelihood
  [current]
  (+ (model current)
     (priors-log-likelihood current)))

(prn {:valid-rectangle          (log-likelihood [0.0 1.0 0.1 0.7])
      :invalid-rectangle        (log-likelihood [-1.0 1.0 0.3 1.3])
      :missing-points-rectangle (log-likelihood [0.1 0.2 0.1 0.3])})

(defn param-step
  [current]
  (mapv #(r/grand % 0.01) current))

(prn (param-step [0 1 0 1]))

(defn sample
  [current]
  (let [new (param-step current)]
    {:state new :score (log-likelihood new)}))

(prn (sample [0.1 0.9 0.1 0.9]))

(defn acceptance-rule
  "Should we accept new points or not?"
  [old-l new-l]
  (or (> new-l old-l)                                       ;; always accept when new score is greater than old one
      (< (r/drand) (m/exp (- new-l old-l)))                 ;; if not, accept with the probability defined by the ratio of likelihoods
      ))

(defn metropolis-hastings
  [init]
  (let [first-step {:state    init :score (log-likelihood init)
                    :accepted [init] :rejected [] :path [init]}]
    (iterate (fn [{:keys [state score accepted rejected path] :as all}]
               (let [{new-state :state
                      new-score :score} (sample state)
                     new-step (if (acceptance-rule score new-score)
                                {:state    new-state
                                 :score    new-score
                                 :accepted (conj accepted new-state)
                                 :rejected rejected
                                 :path     path}
                                (update all :rejected conj new-state))]
                 (update new-step :path conj new-state))) first-step)))

(->> (metropolis-hastings [0 1 0 1])
     (drop 1000000)
     first
     :state)

