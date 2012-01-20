(ns bentley-ottmann.data
  (:require (bentley-ottmann [math :as m])))

(defprotocol STAMPED
  (get-id [this]))

(defprotocol SEGMENT
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
  )

(defprotocol SWEEPLINE
  (add-segment [this sweep-x segment])  ; Add a segment to the sweep line, return new sweep line.
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
  (add-unless-nil-or-present [this p])  ; Add a point (intersection) unless it's nil or already present.
  )

(defprotocol SEGMENTMAP
  (is-left-endpoint [this p])           ; This is the left end point of a line segment.
  (is-right-endpoint [this p])          ; This is the right end point of a line segment.
  (get-segment [this p])                ; Get the line segment containing this point; nil if an intersection.
  (get-map [this])
  )

(deftype Point [id x y]
  STAMPED
  (get-id [this] id)

  POINT
  (pos [this] [x y])
  (get-x [this] x)
  (get-y [this] y)
  (lt [this other] (or (< x (get-x other))
                       (and (= x (get-x other))
                            (< y (get-y other)))))
  (same-pos [this other] (and (= x (get-x other)) (= y (get-y other))))
  
  Object
  ;; Equality by ID, since we need to use individual points as hash keys.
  (equals
    [this other]
    (and (instance? Point other) (= id (get-id other))))

  ;; Ditto for hashCode().
  (hashCode [this] id)

  (toString [this] (str "#" id " (" x ", " y ")")))

(def id-counter (atom 0))

(defn make-Point [x y]
   (Point. (swap! id-counter inc) x y))

(deftype Segment [_id _lo-point _hi-point]
  STAMPED
  (get-id [this] _id)

  SEGMENT
  (lo-point [this] _lo-point)
  (hi-point [this] _hi-point)
  
  (intersect-with [this other]
    (let [p0 [(get-x _lo-point) (get-y _lo-point)]
          p1 [(get-x _hi-point) (get-y _hi-point)]
          p2 [(get-x (lo-point other)) (get-y (lo-point other))]
          p3 [(get-x (hi-point other)) (get-y (hi-point other))]
          result (m/intersection p0 p1 p2 p3)]
      (if (nil? result)
        nil
        (let [[x y] result] (make-Point x y))))
    )

  Object
  (equals [this other]
    (and (instance? Segment other) (= _id (get-id other))))

  (hashCode [this] _id)

  (toString [this] (str "#" _id ": (" _lo-point " => " _hi-point ")")))

(defn make-Segment [p0 p1]
  (let [[p0' p1']
        (if (lt p0 p1) [p0 p1] [p1 p0])]
    (Segment. (swap! id-counter inc) p0' p1')))

;; TODO: deal with segments which are entirely vertical.
(defn intersection-y
  "Return canonical Y intersection point of this segment for a sweep value X."
  [sweep-x p-lo p-hi]
  (let [x-lo [sweep-x, -1]
        x-hi [sweep-x, +1]]
    (nth (m/intersection-projected x-lo x-hi p-lo p-hi) 1)))

(defn sweep-ordered [sweep-x [s0-lo s0-hi] [s1-lo s1-hi]]
  (let [y0 (intersection-y sweep-x s0-lo s0-hi)
        y1 (intersection-y sweep-x s1-lo s1-hi)]
    (< y0 y1)))                         ; TODO: check when points coincide.

(deftype SweepLine [ordered-segs]
  SWEEPLINE
  (add-segment [this sweep-x segment]
    (let [[below above]
          (split-with #(sweep-ordered sweep-x
                                      [(pos (lo-point %1)) (pos (hi-point %1))]
                                      [(pos (lo-point %2)) (pos (hi-point %2))]) ordered-segs)]
      (SweepLine. (concat below (cons segment above)))))

  Object
  (toString [this] (str ordered-segs)))

(defn make-SweepLine [] (SweepLine. []))

;; EndPointList contains (ordered) points but also a map from each (non-intersection) point to its
;; segment, with a flag saying it starts or ends the segment.

(deftype EndPointList [_points]
  ENDPOINTLIST
  (is-empty [this] (empty? _points))

  (split-first [this]
    [(first _points) (EndPointList. (rest _points))])

  Object
  (toString [this] "<ENDPOINTLIST>"))

(defn build-points [segs]
  (concat (map lo-point segs) (map hi-point segs)))

(defn sort-points [pts]
  (sort lt pts))

(defn make-EndPointList
  "Takes an unordered list of segments. Turns them into an ordered point list,
   where each point can be mapped back to its segment."
  [segs]
  (let [pts (sort-points (build-points segs))]
    (EndPointList. pts)))

(deftype SegmentMap [map]
  SEGMENTMAP

  (is-left-endpoint [this p]
    (let [entry (map p)]
      (if (nil? entry) false (= :L (:sense entry)))))
  
  (is-right-endpoint [this p]
    (let [entry (map p)]
      (if (nil? entry) false (= :R (:sense entry)))))
  
  (get-segment [this p]
    (let [entry (map p)]
      (if (nil? entry) nil (:segment entry))))

  ;; Unit testing only:
  (get-map [this] map)

  Object
  (toString [this] (str map)))

(defn build-segment-map [segs]
  (letfn [(f [map seg]
            (let [lo-pt (lo-point seg)
                  hi-pt (hi-point seg)]
              (assoc map
                lo-pt {:sense :L :segment seg}
                hi-pt {:sense :R :segment seg}))
            )]
      (reduce f { } segs)))

(defn make-SegmentMap [segs]
  (SegmentMap. (build-segment-map segs)))
