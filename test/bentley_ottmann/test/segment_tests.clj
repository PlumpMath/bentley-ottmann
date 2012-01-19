(ns bentley-ottmann.test.segment-tests
  (:require (bentley-ottmann [main :as m] [data :as d]))
  (:use [expectations]))

(let [p1 (d/make-Point 0 0)
      p2 (d/make-Point 1 1)
      s1 (d/make-Segment p1 p2)
      s2 (d/make-Segment p2 p1)]
  (expect [0 0] (d/pos (d/lo-point s1)))
  (expect [1 1] (d/pos (d/hi-point s1)))
  (expect [0 0] (d/pos (d/lo-point s2)))
  (expect [1 1] (d/pos (d/hi-point s2))))

(let [p1 (d/make-Point 0 0)
      p2 (d/make-Point 1 1)
      s (d/make-Segment p1 p2)]
  (expect true? (d/same-segment s s)))

(let [p1 (d/make-Point 0 0)
      p2 (d/make-Point 1 1)
      s1 (d/make-Segment p1 p2)
      s2 (d/make-Segment p1 p2)]
  (expect false? (d/same-segment s1 s2)))

(let [lo-seg (d/make-Segment (d/make-Point 0 0) (d/make-Point 1 0))
      hi-seg (d/make-Segment (d/make-Point 0 1) (d/make-Point 1 1))]
  (expect nil? (d/intersect-with lo-seg hi-seg))
  (expect nil? (d/intersect-with hi-seg lo-seg)))

(let [lo-seg (d/make-Segment (d/make-Point 0 0) (d/make-Point 1 1))
      hi-seg (d/make-Segment (d/make-Point 0 1) (d/make-Point 1 0))
      crossing (d/make-Point 0.5 0.5)]
  (expect true? (d/same-pos crossing (d/intersect-with lo-seg hi-seg)))
  (expect true? (d/same-pos crossing (d/intersect-with hi-seg lo-seg))))
