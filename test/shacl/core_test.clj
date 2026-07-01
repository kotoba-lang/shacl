(ns shacl.core-test
  (:require [clojure.test :refer [deftest is]]
            [shacl.core :as shacl]))

(deftest validates-node-shapes
  (let [shape (shacl/node-shape
               (shacl/property-shape {:path :id :min-count 1 :datatype :string})
               (shacl/property-shape {:path :type :in ["Person" "Organization"]}))
        node {:id "alice" :type "Person"}]
    (is (:valid? (shacl/validate shape node)))))

(deftest reports-errors
  (let [shape (shacl/node-shape
               (shacl/property-shape {:path :id :min-count 1})
               (shacl/property-shape {:path :age :datatype :integer}))
        result (shacl/validate shape {:age "old"})]
    (is (false? (:valid? result)))
    (is (= #{:shacl/min-count :shacl/datatype}
           (set (map :error (:errors result)))))))
