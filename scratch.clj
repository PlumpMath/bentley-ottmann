(require '(bentley-ottmann [data :as d]))


(def input-lines
  "Testing: an unordered set of lines."
  #{{:p1 {:x 0.0 :y 0.0} :p2 {:x 1.0 :y 1.0}}
    {:p1 {:x 0.0 :y 1.0} :p2 {:x 1.0 :y 0.0}}})


(d/make-Point 5 6)

(d/make-Segment (d/make-Point 0 0) (d/make-Point 1 1))

(map d/construct input-lines)
