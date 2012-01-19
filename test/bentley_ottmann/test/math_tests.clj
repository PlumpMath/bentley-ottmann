(ns bentley-ottmann.test.math-tests
  (:require (bentley-ottmann [math :as m]))
  (:use [expectations]))

(expect true? (m/in-range 0 1 2))
(expect true? (m/in-range 2 1 0))
(expect true? (m/in-range 0 1/2 1))
(expect true? (m/in-range 0 0.5 1))

(expect false? (m/in-range 1 0 2))
(expect false? (m/in-range 2 0 1))
(expect false? (m/in-range 0.5 0 1))
(expect false? (m/in-range 1/2 0 2))

(expect [1/2 1/2] (m/intersection-projected [0 0] [1 1] [0 1] [1 0]))
