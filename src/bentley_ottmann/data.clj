(ns bentley-ottmann.data)

(defprotocol SEGMENT
  (get-id [this])
  (same-segment [this other])           ; Use for retrieval in the sweep list.
  (intersect-with [this other])        ; Get intersection point, or nil if no intersection.
  )

(defprotocol POINT
  (is-left-endpoint [this])             ; This is the left end point of a line segment.
  (is-right-endpoint [this])            ; This is the right end point of a line segment.
  (get-segment [this])                 ; Get the line segment containing this point, or nil if it's an intersection.
  )

(deftype Segment [id]
  SEGMENT
  (same-segment [this other]
    (= (get-id this) (get-id other)))

  (intersect-with [this other]
    nil)

  Object
  (toString [this] (str id)))

(deftype Point [id x y]
  POINT

  Object
    (toString [this] (str id " (" x ", " y ")")))

(def id-counter (atom 0))

(defn make-Point [x y]
  (Point. (swap! id-counter inc) x y))

(defn make-Segment []
  (Segment. (swap! id-counter inc)))

(defn order-raw-line
  "Order a raw line so that p1 is hit by sweep before p2."
  [{p1 :p1 p2 :p2}]
  (let [{x1 :x y1 :y} p1
        {x2 :x y2 :y} p2]
    (if (< x1 x2)
      {:p1 p1 :p2 p2}
      {:p1 p2 :p2 p1})))

(defn construct
  "Turns a raw line segment (any direction) into low and high points, each able to
   return their comment line segment.
  "
  [line]
  (let
      [{p1 :p1 p2 :p2} (order-raw-line line)
       point-1 (make-Point (:x p1) (:y p1))
       point-2 (make-Point (:x p2) (:y p2))
       seg (make-Segment)
       ]
    {:lo point-1
     :hi point-2}))
