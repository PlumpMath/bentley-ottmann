(ns bentley-ottmann.util)

(defn insert
  "Insert `x` into sorted sequence `xs`, using ordering function lt`."
  [lt x xs]
  (let [[L R] (split-with #(lt % x) xs)]
    (concat L [x] R)))
