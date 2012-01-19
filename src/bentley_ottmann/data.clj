(ns bentley-ottmann.data)

(defprotocol SEGMENT
  (get-id [this])
  (same-segment [this other])           ; Use for retrieval in the sweep list.
  (intersect-with [this other])         ; Get intersection point, or nil if no intersection.
  (lo-point [this])                     ; Get "lowest" point.
  (hi-point [this])                     ; Get "highest" point.
  )

(defprotocol POINT
  (is-left-endpoint [this])             ; This is the left end point of a line segment.
  (is-right-endpoint [this])            ; This is the right end point of a line segment.
  (get-segment [this])                  ; Get the line segment containing this point, or nil if an intersection.
  )

(defprotocol SWEEPLINE
  (add-segment [this segment])          ; Add a segment to the sweep line, return new sweep line.
  (remove-segment [this segment])       ; Remove a segment.
  (segment-above [this segment])        ; Get the segment above this one.
  (segment-below [this segment])        ; Get the segment below this one.
  (ordered-intersecting-segments [this p]) ; Get [lo, hi], the segments intersecting p.
  (swap-segments [this seg1 seg2])      ; Swap these segments.
  )

(defprotocol ENDPOINTLIST
  (is-empty [this])                     ; Is the end point list empty?
  (split-first [this])                  ; Return [first, rest].
  (add-unless-nil [this p])             ; Add a point (intersection) unless it's nil.
  (add-unless-nil-or-present [this p])  ; Add a point unless it's nil or already present.
  )

(deftype Segment [id lo-point hi-point]
  SEGMENT
  (get-id [this] id)
  
  (same-segment [this other]
    (= id (get-id other)))

  (intersect-with [this other]
    nil)

  Object
  (toString [this] (str "#" id ": (" lo-point " => " hi-point ")")))

(deftype Point [id x y]
  POINT

  Object
    (toString [this] (str "#" id " (" x ", " y ")")))

(def id-counter (atom 0))

(defn make-Point [x y]
  (Point. (swap! id-counter inc) x y))

(defn make-Segment
  "NOTE: points must be ordered."
  [p1 p2]
  (Segment. (swap! id-counter inc) p1 p2))

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
       lo-point (make-Point (:x p1) (:y p1))
       hi-point (make-Point (:x p2) (:y p2))]
    (make-Segment lo-point hi-point)))
