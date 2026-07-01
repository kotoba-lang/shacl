(ns shacl.core
  "Small EDN shape validator inspired by SHACL Core.")

(defn values-at [node path]
  (let [v (get-in node (if (sequential? path) path [path]))]
    (cond
      (nil? v) []
      (sequential? v) (vec v)
      :else [v])))

(defn datatype-match? [datatype v]
  (case datatype
    :string (string? v)
    :number (number? v)
    :integer (integer? v)
    :boolean (boolean? v)
    :keyword (keyword? v)
    :map (map? v)
    :vector (vector? v)
    true))

(defn property-errors [node {:keys [path min-count max-count datatype in node-kind] :as shape}]
  (let [vs (values-at node path)
        c (count vs)]
    (vec
     (concat
      (when (and min-count (< c min-count))
        [{:error :shacl/min-count :path path :expected min-count :actual c}])
      (when (and max-count (> c max-count))
        [{:error :shacl/max-count :path path :expected max-count :actual c}])
      (for [v vs :when (and datatype (not (datatype-match? datatype v)))]
        {:error :shacl/datatype :path path :expected datatype :actual v})
      (for [v vs :when (and in (not (contains? (set in) v)))]
        {:error :shacl/in :path path :allowed (vec in) :actual v})
      (for [v vs :when (and (= node-kind :iri) (not (and (map? v) (= :iri (:rdf/type v)))))]
        {:error :shacl/node-kind :path path :expected :iri :actual v})
      (for [v vs :when (and (= node-kind :literal) (not (and (map? v) (= :literal (:rdf/type v)))))]
        {:error :shacl/node-kind :path path :expected :literal :actual v})))))

(defn node-shape [& property-shapes]
  {:shacl/type :node-shape
   :property (vec property-shapes)})

(defn property-shape [opts]
  opts)

(defn validate [shape node]
  (let [errors (mapcat #(property-errors node %) (:property shape))]
    {:valid? (empty? errors) :errors (vec errors)}))
