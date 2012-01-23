(ns bentley-ottmann.test.util-tests
  (:require (bentley-ottmann [util :as u]))
  (:use [expectations]))

(expect [1 2 3 4 5] (u/insert < 3 [1 2 4 5]))
(expect [1 2 3 4 5] (u/insert < 1 [2 3 4 5]))
(expect [1 2 3 4 5] (u/insert < 5 [1 2 3 4]))
