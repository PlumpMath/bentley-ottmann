(ns bentley-ottmann.util)

(defn insert
  "Insert `x` into sorted sequence `xs`, using ordering function lt`."
  [lt x xs]
  (let [[L R] (split-with #(lt % x) xs)]
    (concat L [x] R)))

(defn insert-unless-present
  [lt x xs]
  (let [[L R] (split-with #(lt % x) xs)]
    (if (= x (first R))
      xs
      (concat L [x] R))))

(defn prlist [L]
  (str "[" (reduce #(if (empty? %1) (str %2) (str %1 ", " %2)) "" L) "]"))

(defn debug [prefix val]
  (println (str "**" (format "%20s" prefix) " :: " (if (nil? val) "nil" val)))
  val)
