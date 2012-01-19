(ns bentley-ottmann.data
  (:require (bentley-ottmann [math :as m])))

(defprotocol SEGMENT
  (get-id [this])
  (same-segment [this other])           ; Use for retrieval in the sweep list.
  (intersect-with [this other])         ; Get intersection point, or nil if no intersection.
  (lo-point [this])                     ; Get "lowest" point.
  (hi-point [this])                     ; Get "highest" point.
  )

(defprotocol POINT
  (pos [this])                          ; Point's position as [x y].
  (get-x [this])                        ; Get x position.
  (get-y [this])                        ; Get y position.
  (lt [this other])                     ; This point is before ("less than") other.
  (same-pos [this other])               ; This point is at the same position as the other point.
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

(deftype Segment [_id _lo-point _hi-point]
  SEGMENT
  (get-id [this] _id)

  (lo-point [this] _lo-point)
  (hi-point [this] _hi-point)
  
  (same-segment [this other]
    (= _id (get-id other)))

  (intersect-with [this other]
    (let [p0 [(get-x _lo-point) (get-y _lo-point)]
          p1 [(get-x _hi-point) (get-y _hi-point)]
          p2 [(get-x (lo-point other)) (get-y (lo-point other))]
          p3 [(get-x (lo-point other)) (get-y (lo-point other))]]
      (m/intersection p0 p1 p2 p3))
    )

  Object
  (toString [this] (str "#" _id ": (" _lo-point " => " _hi-point ")")))

(deftype Point [id x y]
  POINT
  (pos [this] [x y])
  (get-x [this] x)
  (get-y [this] y)
  (lt [this other] (or (< x (get-x other))
                       (and (= x (get-x other))
                            (< y (get-y other)))))
  (same-pos [this other] (and (= x (get-x other)) (= y (get-y other))))
  
  Object
  (toString [this] (str "#" id " (" x ", " y ")")))

(def id-counter (atom 0))

(defn make-Point [x y]
   (Point. (swap! id-counter inc) x y))

(defn make-Segment [p1 p2]
  (let [[p1' p2']
        (if (lt p1 p2) [p1 p2] [p2 p1])]
   (Segment. (swap! id-counter inc) p1' p2')))
