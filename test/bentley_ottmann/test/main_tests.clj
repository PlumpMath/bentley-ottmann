(ns bentley-ottmann.test.main-tests
  (:require (bentley-ottmann [main :as m] [data :as d]))
  (:use [expectations]))

(expect [] (m/bentley-ottmann (d/make-SegmentMap []) (d/make-EndPointList []) (d/make-SweepLine) []))

(expect true? (d/same-pos [d/make-Point 1/2 1/2]
                          (let [p1 (d/make-Point 0 0)
                                p2 (d/make-Point 1 1)
                                p3 (d/make-Point 0 1)
                                p4 (d/make-Point 1 0)
                                s1 (d/make-Segment p1 p2)
                                s2 (d/make-Segment p3 p4)]
                            (m/bentley-ottmann (d/make-SegmentMap [s1 s2])
                                               (d/make-EndPointList [s1 s2])
                                               (d/make-SweepLine)
                                               []))))
