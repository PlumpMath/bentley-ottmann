(ns bentley-ottmann.test.point-tests
  (:require (bentley-ottmann [data :as d]))
  (:use [expectations]))

(expect true? (d/same-pos (d/make-Point 1 1) (d/make-Point 1 1)))

(expect true? (let [p (d/make-Point 0 0)] (= p p)))

(expect false? (= (d/make-Point 1 1) (d/make-Point 1 1)))

(expect false? (= (d/make-Point 0 0) "Hello"))
