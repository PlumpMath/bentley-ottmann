(ns bentley-ottmann.test.segment-tests
  (:require (bentley-ottmann [data :as d]))
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
      s1 (d/make-Segment p1 p2)
      s2 (d/make-Segment p1 p2)]
  (expect false? (= s1 s2)))

(let [lo-seg (d/make-Segment (d/make-Point 0 0) (d/make-Point 1 0))
      hi-seg (d/make-Segment (d/make-Point 0 1) (d/make-Point 1 1))]
  (expect nil? (d/intersect-with lo-seg hi-seg))
  (expect nil? (d/intersect-with hi-seg lo-seg)))

(let [lo-seg (d/make-Segment (d/make-Point 0 0) (d/make-Point 1 1))
      hi-seg (d/make-Segment (d/make-Point 0 1) (d/make-Point 1 0))
      crossing (d/make-Point 1/2 1/2)]
  (expect true? (d/same-pos crossing (d/intersect-with lo-seg hi-seg)))
  (expect true? (d/same-pos crossing (d/intersect-with hi-seg lo-seg))))

(let [p00 (d/make-Point 0 0)
      p11 (d/make-Point 1 1)
      seg (d/make-Segment p00 p11)
      seg-map (d/make-SegmentMap [seg])]
  (expect {p00 {:sense :L :segment seg}
           p11 {:sense :R :segment seg}} (d/get-map seg-map)))

(let [p00 (d/make-Point 0 0)
      p11 (d/make-Point 1 1)
      p01 (d/make-Point 0 1)
      p10 (d/make-Point 1 0)
      lo-seg (d/make-Segment p00 p11)
      hi-seg (d/make-Segment p01 p10)
      seg-map (d/make-SegmentMap [lo-seg hi-seg])]
  (expect {p00 {:sense :L :segment lo-seg}
           p11 {:sense :R :segment lo-seg}
           p01 {:sense :L :segment hi-seg}
           p10 {:sense :R :segment hi-seg}} (d/get-map seg-map)))
