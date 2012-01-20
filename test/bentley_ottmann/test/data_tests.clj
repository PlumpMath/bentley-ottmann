(ns bentley-ottmann.test.data-tests
  (:require (bentley-ottmann [data :as d]))
  (:use [expectations]))

(expect 1 (d/intersection-y 1 [0 0] [2 2]))

(expect 2 (d/intersection-y 2 [0 0] [1 1]))

(let [seg0 [[0 0] [2 2]]
      seg1 [[0 2] [2 0]]]
  (expect true?  (d/sweep-ordered   0 seg0 seg1))
  (expect true?  (d/sweep-ordered 1/2 seg0 seg1))
  (expect false? (d/sweep-ordered 3/2 seg0 seg1))
  (expect false? (d/sweep-ordered   2 seg0 seg1)))
