(ns bentley-ottmann.math)

(defn canonical-line
  "Turn line from p0 to p1 into [A B C] for Ax + By = C."
  [p0 p1]
  (let [[x0 y0] p0
        [x1 y1] p1
        a (- y1 y0)
        b (- x0 x1)]
    [a b (+ (* a x0) (* b y0))]))

(defn intersection-projected
  "A cleaner algorithm. Projects intersection point beyond any line ending
  
   http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2"
  
  [p0 p1 p2 p3]

  (let [[a1 b1 c1] (canonical-line p0 p1)
        [a2 b2 c2] (canonical-line p2 p3)
        det (- (* a1 b2) (* a2 b1))]
    (if (= det 0)
      nil
      [(/ (- (* b2 c1) (* b1 c2)) det)
       (/ (- (* a1 c2) (* a2 c1)) det)])))

(defn in-range [lo n hi]
  (or (and (>= n lo) (<= n hi))
      (and (>= n hi) (<= n lo))))

(defn intersection
  [p0 p1 p2 p3]

  (let [pt (intersection-projected p0 p1 p2 p3)]
    (if (nil? pt)
      nil
      (let [[ix iy] pt
            [p0_x p0_y] p0
            [p1_x p1_y] p1
            [p2_x p2_y] p2
            [p3_x p3_y] p3]
        (if (and (in-range p0_x ix p1_x)
                 (in-range p0_y iy p1_y)
                 (in-range p2_x ix p3_x)
                 (in-range p2_y iy p3_y))
          pt
          nil)))))

;; TODO: deal with segments which are entirely vertical.
(defn intersection-y
  "Return canonical Y intersection point of this segment for a sweep value X."
  [sweep-x p-lo p-hi]
  (let [x-lo [sweep-x, -1]
        x-hi [sweep-x, +1]]
    (nth (intersection-projected x-lo x-hi p-lo p-hi) 1)))

(defn sweep-ordered
  "Determine whether two segments (as raw data) are in increasing Y order at a sweep position X."
  [sweep-x [s0-lo s0-hi] [s1-lo s1-hi]]
  (let [y0 (intersection-y sweep-x s0-lo s0-hi)
        y1 (intersection-y sweep-x s1-lo s1-hi)]
    (< y0 y1)))                         ; TODO: check when points coincide.
