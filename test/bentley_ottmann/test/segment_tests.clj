(ns bentley-ottmann.test.segment-tests
  (:require (bentley-ottmann [main :as m] [data :as d]))
  (:use [clojure.test]))

(testing "segments"
  (deftest seg-eq
    (let [s (d/make-Segment)]
      (is (d/same-segment s s))))

  (deftest seg-noteq
    (let [s1 (d/make-Segment)
          s2 (d/make-Segment)]
      (is (not (d/same-segment s1 s2)))))

  (deftest intersection
    
    ))
