(ns bentley-ottmann.core
  (:require (bentley-ottmann [data :as d])))

(defn iterate
  "`endpoint-list` is an ordered list of points. If it's a left or a right
   end point, it can be mapped to its line segment; otherwise, it's an
   internal intersection point, both of whose lines are currently in the
   sweep line.

   `sweep-line` is an ordered list of lines, such that intersections can only
   occur between neighbours.

   `output-list` is a list of intersection points. It doesn't need to be sorted,
   since it isn't examined once populated.
  "
  [endpoint-list sweep-line output-list]
  (if (empty endpoint-list)
    output-list
    (let [E (first endpoint-list)]
      (cond (d/is-left-endpoint E)
            (let [seg-E (get-segment E)
                  sweep-line' (add seg-E sweep-line)
                  seg-A (segment-above seg-E sweep-line')
                  seg-B (segment-below seg-E sweep-line')
                  i1-opt (intersect-with seg-E seg-A)
                  i2-opt (intersect-with seg-E seg-B)
                  endpoint-list' (poss-insert i1-opt (poss-insert i2-opt (rest endpoint-list)))]
              (recur endpoint-list' sweep-line' output-list))

            (d/is-right-endpoint E)
            (let [seg-E (get-segment E)
                  seg-A (segment-above seg-E sweep-line)
                  seg-B (segment-below seg-E sweep-line)
                  sweep-line' (remove seg-E sweep-line)
                  i-opt (intersect-with seg-A seg-B)
                  endpoint-list' (poss-insert-if-not-already-present i-opt endpoint-list)]
              (recur endpoint-list' sweep-line' output-list))

            :else
            (let [[segE1 segE2] (ordered-intersecting-segments E sweep-line)
                  sweep-line' (swap segE1 segE2 sweep-line)
                  segA (segment-above segE2 sweep-line')
                  segB (segment-below segE1 sweep-line')
                  i1-opt (intersect-with segE2 segA)
                  i2-opt (intersect-with segE1 segB)
                  endpoint-list' (poss-insert-if-not-already-present i1-opt endpoint-list)
                  endpoint-list'' (poss-insert-if-not-already-present i2-opt endpoint-list')]
              (recur endpoint-list'' sweep-line' (add E output-list)))))))
