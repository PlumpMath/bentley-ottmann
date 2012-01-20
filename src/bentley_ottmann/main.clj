(ns bentley-ottmann.main
  (:require (bentley-ottmann [data :as d])))

(defn bentley-ottmann
  "`segment-map` maps each non-intersect point to its segment, and knows whether
   the point starts or ends the segment.

   `endpoint-list` is an ordered list of points. If it's a left or a right
   end point, it can be mapped to its line segment; otherwise, it's an
   internal intersection point, both of whose lines are currently in the
   sweep line.

   `sweep-line` is an ordered list of lines, such that intersections can only
   occur between neighbours.

   `output-list` is a list of intersection points. It doesn't need to be sorted,
   since it isn't examined once populated."

  [segment-map endpoint-list sweep-line output-list]
  (if (d/is-empty endpoint-list)
    output-list
    (let [[E E-rest] (d/split-first endpoint-list)]
      (cond (d/is-left-endpoint segment-map E)
            (let [seg-E (d/get-segment segment-map E)
                  sweep-x (d/get-x E)
                  sweep-line' (d/add-segment sweep-line sweep-x seg-E)
                  seg-A (d/segment-above sweep-line' seg-E)
                  seg-B (d/segment-below sweep-line' seg-E)
                  i1-opt (d/intersect-with seg-E seg-A)
                  i2-opt (d/intersect-with seg-E seg-B)
                  endpoint-list' (d/add-unless-nil i1-opt (d/add-unless-nil i2-opt E-rest))]
              (recur segment-map endpoint-list' sweep-line' output-list))

            (d/is-right-endpoint segment-map E)
            (let [seg-E (d/get-segment segment-map E)
                  seg-A (d/segment-above seg-E sweep-line)
                  seg-B (d/segment-below seg-E sweep-line)
                  sweep-line' (d/remove-segment sweep-line seg-E)
                  i-opt (d/intersect-with seg-A seg-B)
                  endpoint-list' (d/add-unless-nil-or-present i-opt E-rest)]
              (recur segment-map endpoint-list' sweep-line' output-list))

            :else                       ; Intersection point.
            (let [[segE1 segE2] (d/ordered-intersecting-segments sweep-line E)
                  sweep-line' (d/swap-segments sweep-line segE1 segE2)
                  segA (d/segment-above segE2 sweep-line')
                  segB (d/segment-below segE1 sweep-line')
                  i1-opt (d/intersect-with segE2 segA)
                  i2-opt (d/intersect-with segE1 segB)
                  endpoint-list' (d/add-unless-nil-or-present i1-opt E-rest)
                  endpoint-list'' (d/add-unless-nil-or-present i2-opt endpoint-list')]
              (recur segment-map endpoint-list'' sweep-line' (conj E output-list)))))))
