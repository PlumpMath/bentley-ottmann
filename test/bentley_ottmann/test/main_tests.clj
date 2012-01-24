(ns bentley-ottmann.test.main-tests
  (:require (bentley-ottmann [util :as u]
                             [main :as m]
                             [data :as d]))
  (:use [expectations]))

(expect [] (m/bentley-ottmann (d/make-SegmentMap []) (d/make-EndPointList []) (d/make-SweepLine) []))

(defn doit [raw-segs]
  (let [segments (map (fn [[[p0-x p0-y] [p1-x p1-y]]]
                        (d/make-Segment (d/make-Point p0-x p0-y) (d/make-Point p1-x p1-y)))
                      raw-segs)
        results (m/bentley-ottmann (d/make-SegmentMap segments)
                                   (d/make-EndPointList segments)
                                   (d/make-SweepLine)
                                   [])]
    (map d/pos results)))

(expect [] (doit []))

(expect [[1/2 1/2]]
        (doit [[[0 0] [1 1]]
               [[0 1] [1 0]]]))

(expect [[1/2 1/2]]
        (doit [[[0 1] [1 0]]
               [[0 0] [1 1]]]))

(expect []
        (doit [[[0 0] [1 1]]]))

(expect []
        (doit [[[0 0] [1 0]]
               [[0 1] [1 1]]]))

(expect [[1/2 1/2]]
        (doit [[[0 0] [1 1]]
               [[0 1] [1 0]]]))

(expect [[1/2 1/2] [3/2 3/2]]
        (doit [[[0 0] [2 2]]
               [[0 1] [1 0]]
               [[1 2] [2 1]]]))
