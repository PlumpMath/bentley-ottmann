(ns bentley-ottmann.main
  (:require (bentley-ottmann [util :as u]
                             [data :as d])))

(defn bentley-ottmann-1
  "One iteration of Bentley-Ottman.

   `segment-map` maps each non-intersect point to its segment, and knows whether
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
    (let [[E E-rest] (u/debug "Processing" (d/split-first endpoint-list))]
      (cond (d/is-left-endpoint segment-map E)
            (let [seg-E (u/debug "(LEFT) seg-E" (d/get-segment segment-map E))
                  sweep-x (d/get-x E)
                  sweep-line' (u/debug "sweep-line'" (d/add-segment sweep-line sweep-x seg-E))
                  seg-A (u/debug "seg-A" (d/segment-above sweep-line' seg-E))
                  seg-B (u/debug "seg-B" (d/segment-below sweep-line' seg-E))
                  i1-opt (u/debug "i1-opt" (d/intersect-with seg-E seg-A))
                  i2-opt (u/debug "i2-opt" (if (nil? seg-B) nil (d/intersect-with seg-B seg-E)))
                  endpoint-list' (u/debug "epl'" (d/add-unless-nil (d/add-unless-nil E-rest i2-opt) i1-opt))]
              [endpoint-list' sweep-line' output-list])

            (d/is-right-endpoint segment-map E)
            (let [seg-E (u/debug "(RIGHT) seg-E" (d/get-segment segment-map E))
                  seg-A (d/segment-above sweep-line seg-E)
                  seg-B (d/segment-below sweep-line seg-E)
                  sweep-line' (d/remove-segment sweep-line seg-E)
                  i-opt (if (nil? seg-B) nil (d/intersect-with seg-B seg-A))
                  endpoint-list' (d/add-unless-nil-or-present E-rest i-opt)]
              [endpoint-list' sweep-line' output-list])

            :else                       ; Intersection point.
            (let [;;[segE1 segE2] (d/ordered-intersecting-segments sweep-line E)
                  [segE2 segE1] (u/debug "SURROUNDING" (d/get-lo-hi-segs E))
                  sweep-line' (u/debug "swapped" (d/swap-segments sweep-line segE1 segE2))
                  segA (d/segment-above sweep-line' segE2)
                  segB (d/segment-below sweep-line' segE1)
                  i1-opt (u/debug "i1-opt" (d/intersect-with segE2 segA))
                  i2-opt (u/debug "i2-opt" (if (nil? segB) nil (d/intersect-with segB segE1)))
                  endpoint-list' (u/debug "epl'" (d/add-unless-nil-or-present E-rest i1-opt))
                  endpoint-list'' (u/debug "epl''" (d/add-unless-nil-or-present endpoint-list' i2-opt))]
              [endpoint-list'' sweep-line' (u/debug "output-list" (conj output-list E))])))))

(defn bentley-ottmann
  [segment-map endpoint-list sweep-line output-list]
  (if (d/is-empty endpoint-list)
    output-list
    (let [[endpoint-list' sweep-line' output-list']
          (u/debug "PASS" (bentley-ottmann-1 segment-map endpoint-list sweep-line output-list))]
      (recur segment-map endpoint-list' sweep-line' output-list'))))
