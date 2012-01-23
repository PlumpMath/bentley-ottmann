(require '(bentley-ottmann [data :as d]
                           [math :as m]))

(import '(bentley_ottmann.data SweepLine))

(def input-lines
  "Testing: an unordered set of lines."
  #{{:p1 {:x 0.0 :y 0.0} :p2 {:x 1.0 :y 1.0}}
    {:p1 {:x 0.0 :y 1.0} :p2 {:x 1.0 :y 0.0}}})


(d/make-Point 5 6)

(d/make-Segment (d/make-Point 0 0) (d/make-Point 1 1))

(map d/construct input-lines)

(m/intersection-projected [0 0] [1 1] [0 1] [1 0])

(m/in-range 0 1/2 1)

(m/intersection [0 0] [1 0] [0 1] [1 1])

(def lo-seg (d/make-Segment (d/make-Point 0 0) (d/make-Point 1 1)))

(def hi-seg (d/make-Segment (d/make-Point 0 1) (d/make-Point 1 0)))

hi-seg

(d/intersect-with lo-seg hi-seg)

(sort-by identity [4 5 2 10 -1])

(sort < [4 5 2 10 -1])

(= {1 "1" 3 "3" 2 "2" 4 "4"} {4 "4" 3 "3" 2 "2" 1 "1"})

(= [1 2 3] '(1 2 3))

(split-with #(<= % 0) [-2 -1 0 1 2 1 0 -1 -2])

(drop-while (fn [_] nil) [0 1 2 3 4 5])

(concat [1 2 3] [ 4] [5 6 7])

(let [[a b]
      (split-with (partial not= 3) '( 1 2 3 4 5))]
  (first (rest  b)))

(split-with (partial not= 3) [])

;;---
(require '(bentley-ottmann [data :as d]
                           [math :as m]))

(import '(bentley_ottmann.data SweepLine))

(def p1 (d/make-Point 0 0))
(def p2 (d/make-Point 1 1))
(def p3 (d/make-Point 0 1))
(def p4 (d/make-Point 1 0))
(def s1 (d/make-Segment p1 p2))
(def s2 (d/make-Segment p3 p4))
(def seg-map (d/make-SegmentMap [s1 s2]))
(def end-point-list (d/make-EndPointList [s1 s2]))
(def sl-2 (d/add-segment (d/make-SweepLine) 0 s1))
(def sl-3 (d/add-segment sl-2 0 s2))

(def native (SweepLine. [s1]))
;;---
(d/examine (d/add-segment sl-2 0 s2))
(d/examine sl-2)

(d/examine (d/add-segment native 0 s2))

native

sl-2
sl-3

(first (rest nil))
(defprotocol FOOBLE
  (add [this item]))

(deftype Fooble [items]
  FOOBLE
  (add [this item]
    (Fooble. (cons item items)))

  Object
  (toString [this] (str items))
  )

(add (add (Fooble. []) 4) 6)



(d/segment-above sl-2 s1)
(d/segment-below sl-2 s1)

(concat [3] (cons 3 [4 5 6] ))

end-point-list

(cons 3 '())1