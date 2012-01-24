(ns bentley-ottmann.data
  (:require (bentley-ottmann [math :as m]
                             [util :as u])))

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
  (get-lo-hi-segs [this])               ; Return [lo hi], the below and above segments at intersection, or nil if N/A.
  )

(defprotocol SWEEPLINE
  (add-segment [this sweep-x segment])  ; Add a segment to the sweep line, return new sweep line.
  (remove-segment [this segment])       ; Remove a segment.
  (segment-above [this segment])        ; Get the segment above this one.
  (segment-below [this segment])        ; Get the segment below this one.
  (swap-segments [this seg1 seg2])      ; Swap these segments.
  )

(defprotocol ENDPOINTLIST
  (is-empty [this])                     ; Is the end point list empty?
  (split-first [this])                  ; Return [first, rest].
  (add-unless-nil [this p])             ; Add a point (intersection) unless it's nil.
  (add-unless-nil-or-present [this p])  ; Add a point (intersection) unless it's nil or already present.
  (get-data [this])
  )

(defprotocol SEGMENTMAP
  (is-left-endpoint [this p])           ; This is the left end point of a line segment.
  (is-right-endpoint [this p])          ; This is the right end point of a line segment.
  (get-segment [this p])                ; Get the line segment containing this point; nil if an intersection.
  (get-map [this])
  )

(deftype Point [id x y lo-hi-segs-opt]
  STAMPED
  (get-id [this] id)

  POINT
  (pos [this] [x y])
  (get-x [this] x)
  (get-y [this] y)
  ;; Ordering principally by increasing x, then increasing y.
  (lt [this other] (or (< x (get-x other))
                       (and (= x (get-x other))
                            (< y (get-y other)))))

  (same-pos [this other] (and (= x (get-x other)) (= y (get-y other))))

  (get-lo-hi-segs [this] lo-hi-segs-opt)
  
  Object
  ;; Equality by ID, since we need to use individual points as hash keys.
  (equals
    [this other]
    (and (instance? Point other) (= id (get-id other))))

  ;; Ditto for hashCode().
  (hashCode [this] id)

  (toString [this] (str "(#" id ": " x ", " y
                        (if (nil? lo-hi-segs-opt)
                          ""
                          (let [[lo hi] lo-hi-segs-opt] (str " {#" (get-id lo) "^#" (get-id hi) "}")))
                        ")")))

(def id-counter (atom 0))

(defn make-Point
  ([x y]
     (Point. (swap! id-counter inc) x y nil))
  ([x y seg-below seg-above]
     (Point. (swap! id-counter inc) x y [seg-below seg-above])))

(deftype Segment [_id _lo-point _hi-point]
  STAMPED
  (get-id [this] _id)

  SEGMENT
  (lo-point [this] _lo-point)
  (hi-point [this] _hi-point)
  
  (intersect-with [this other]          ; NB: leftmost of this < leftmost of other.
    (if (nil? other)
      nil
      (let [p0 [(get-x _lo-point) (get-y _lo-point)]
            p1 [(get-x _hi-point) (get-y _hi-point)]
            p2 [(get-x (lo-point other)) (get-y (lo-point other))]
            p3 [(get-x (hi-point other)) (get-y (hi-point other))]
            result (m/intersection p0 p1 p2 p3)]
        (if (nil? result)
          nil
          (let [[x y] result] (make-Point x y this other))))))

  Object
  (equals [this other]
    (and (instance? Segment other) (= _id (get-id other))))

  (hashCode [this] _id)

  (toString [this] (str "#" _id ": (" _lo-point " => " _hi-point ")")))

(defn make-Segment
  "Create a segment from two points, ordering them if necessary."
  [p0 p1]
  (let [[p0' p1']
        (if (lt p0 p1) [p0 p1] [p1 p0])]
    (Segment. (swap! id-counter inc) p0' p1')))

(deftype SweepLine [ordered-segs]
  SWEEPLINE
  (add-segment [this sweep-x segment]
    (let [[below above]
          (split-with #(m/sweep-ordered sweep-x
                                      [(pos (lo-point %)) (pos (hi-point %))]
                                      [(pos (lo-point segment)) (pos (hi-point segment))]) ordered-segs)]
      (SweepLine. (concat below (cons segment above)))))

  (remove-segment [this segment]
    (SweepLine. (remove (partial = segment) ordered-segs)))

  (segment-above [this segment]
    (let [[below above]
          (split-with (partial not= segment) ordered-segs)]
      (first (next above))))

  (segment-below [this segment]
    (let [[below above]
          (split-with (partial not= segment) ordered-segs)]
      (last below)))

  (swap-segments [this seg1 seg2]
    (SweepLine. (replace {seg1 seg2
                          seg2 seg1} ordered-segs)))

  Object
  (toString [this] (u/prlist ordered-segs)))

(defn make-SweepLine [] (SweepLine. []))

;; EndPointList contains (ordered) points but also a map from each (non-intersection) point to its
;; segment, with a flag saying whether it starts or ends the segment.

(deftype EndPointList [_points]
  ENDPOINTLIST
  (is-empty [this] (empty? _points))

  (split-first [this]
    [(first _points) (EndPointList. (rest _points))])

  (add-unless-nil [this p]
    (if (nil? p)
      this
      (EndPointList. (u/insert lt p _points))))

  (add-unless-nil-or-present [this p]
    (if (nil? p)
      this
      (EndPointList. (u/insert-unless-present lt p _points))))

  (get-data [this] _points)

  Object
  (toString [this] (u/prlist _points)))

(defn build-points [segs]
  (sort lt (concat (map lo-point segs) (map hi-point segs))))

(defn make-EndPointList
  "Takes an unordered list of segments. Turns them into an ordered point list,
   where each point can be mapped back to its segment."
  [segs]
  (let [pts (build-points segs)]
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
