(ns bentley-ottmann.test.main-tests
  (:require (bentley-ottmann [main :as m] [data :as d]))
  (:use [expectations]))

(expect [] (m/bentley-ottmann (d/make-SegmentMap []) (d/make-EndPointList []) (d/make-SweepLine) []))

(defn seg [[p0-x p0-y] [p1-x p1-y]]
  (let [p0 (d/make-Point p0-x p0-y)
        p1 (d/make-Point p1-x p1-y)]
    (d/make-Segment p0 p1)))

(defn iter [n seg-map [epl sw out]]
  (if (zero? n)
    [epl sw out]
    (recur (dec n) seg-map (m/bentley-ottmann-1 seg-map epl sw out))))

(def s1 (seg [0 0] [1 1]))
(def s2 (seg [0 1] [1 0]))

(let [seg-map (d/make-SegmentMap [s1 s2])
      [epl sw out] (iter 1 seg-map [(d/make-EndPointList [s1 s2])
                                    (d/make-SweepLine)
                                    []])]
  (doall (map println ["--- >>>" epl sw out "--- <<<"])))

(expect true? (d/same-pos (d/make-Point 1/2 1/2)
                          (let [s1 (seg [0 0] [1 1])
                                s2 (seg [0 1] [1 0])]
                            (first (m/bentley-ottmann (d/make-SegmentMap [s1 s2])
                                                      (d/make-EndPointList [s1 s2])
                                                      (d/make-SweepLine)
                                                      [])))))
