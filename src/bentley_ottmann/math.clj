(ns bentley-ottmann.math)

(defn intersection-OLD
  "Calculate intersection of line segment p0-p1 with segment p2-p3. Return [x y]
   or nil if no intersection.

   Algorithm lifted from Andre LeMothe's 'Tricks of the Windows Game Programming Gurus' (it says here)."
  [p0 p1 p2 p3]

  (let [[p0_x p0_y] p0
        [p1_x p1_y] p1
        [p2_x p2_y] p2
        [p3_x p3_y] p3

        s1_x (- p1_x p0_x)
        s1_y (- p1_y p0_y)
        s2_x (- p3_x p2_x)
        s2_y (- p3_y p2_y)

        s (/ (+ (* (- s1_y) (- p0_x p2_x)) ( * s1_x (- p0_y p2_y))) (+ (* (- s2_x) s1_y) (* s1_x s2_y)))
        t (/ (- (*    s2_x  (- p0_y p2_y)) ( * s2_y (- p0_x p2_x))) (+ (* (- s2_x) s1_y) (* s1_x s2_y)))]

    [s t]))

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
