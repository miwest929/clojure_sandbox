(ns neural.core
  (:gen-class :main true))


; activation-fn must be a function that returns 1 or -1 given a Float
;(defn compute-perceptron [inputs weights activation-fn]
;  (activation-fn (->> inputs (map * weights) (reduce +))))

; A perceptron is just it's weights and its activation function
(def learning-rate 0.05)
(defn sign-positive? [output] (if (> output 0) 1 -1))
(defn rand-between-neg1-pos1 [] (- (rand 2) 1))
(defn construct-perceptron [input-count activation-fn]
  {:weights (take input-count (repeatedly #(rand-between-neg1-pos1)))
   :activation-fn activation-fn})

; Returns a new perceptron with updated weights
(defn update-weights [perceptron error inputs]
  (let [weights (perceptron :weights)]
    {:weights (map #(+ %1 (* learning-rate error %2)) weights inputs)
     :activation-fn (perceptron :activation-fn)}))

(defn feedforward [perceptron inputs]
  (let [activation-fn (perceptron :activation-fn) weights (perceptron :weights)]
    (activation-fn (->> inputs (map * weights) (reduce +)))))

(defn error-perceptron [correct actual] (- correct actual))
(defn train-perceptron [perceptron inputs correct]
  (let [actual (feedforward perceptron inputs) error (error-perceptron correct actual)]
    (update-weights perceptron error inputs)))

(def my-perceptron (construct-perceptron 3 sign-positive?))

(def width 800)
(def height 600)
(defn line [x] (+ (* 2 x) 1))
(defn random-pt [] [(rand width) (rand height)])
(defn above-line [yline y]
  (if (< y yline) -1 1))

(defn training [perceptron inputs outputs]
  (if (empty? inputs) perceptron
    (let [next-input (first inputs)
          next-output (first outputs)
          next-perceptron (train-perceptron perceptron next-input next-output)]
      (recur next-perceptron (rest inputs) (rest outputs)))))
(def examples [
  [2 3 1]
  [1 3 1]
])
(def example-outputs (map #(above-line (line (first %)) (nth % 1)) examples))

(defn -main
  "Eventually I will be able to train a multi-layered perceptron (neural network) for you."
  []
  (println (training my-perceptron examples example-outputs)))
  ;(println (feedforward my-perceptron [2 8 1])))

