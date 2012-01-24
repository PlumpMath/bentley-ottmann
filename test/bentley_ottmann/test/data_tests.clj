(ns bentley-ottmann.test.data-tests
  (:require (bentley-ottmann [data :as d]))
  (:use [expectations]))

(let [p00 (d/make-Point 0 0)
      p11 (d/make-Point 1 1)]
  (expect []
          (d/get-data (d/add-unless-nil (d/make-EndPointList []) nil)))
  
  (expect [p00]
          (d/get-data (d/add-unless-nil
                       (d/add-unless-nil (d/make-EndPointList []) nil)
                       p00)))

  (expect [p00]
          (d/get-data (d/add-unless-nil
                       (d/add-unless-nil (d/make-EndPointList []) p00)
                       nil)))

  (expect [p00 p11]
          (d/get-data (d/add-unless-nil
                       (d/add-unless-nil (d/make-EndPointList []) p11)
                       p00)))
  (expect [p00 p11]
          (d/get-data (d/add-unless-nil
                       (d/add-unless-nil (d/make-EndPointList []) p00)
                       p11))))
