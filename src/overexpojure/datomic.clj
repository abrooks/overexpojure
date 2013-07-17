(ns overexpojure.datomic
  (require [datomic.api :as d :refer [db q]]))

(def schema
  (->>
   {:conference [[:name :string] ; ex: "Clojure/conj 2010"
                 [:location :string]
                 [:year :long]
                 [:tags :string :many]] ; ex: "clojure" "functional programming"
    :speaker [[:name :string]
              [:email :string :many]
              [:url :string :many]]
    :talk [[:name :string]
           [:link :string]
           [:conference :ref]
           [:speaker :ref]]}
   (map (fn [[namespace attrs]]
          (for [vattr attrs
                :let [[attr type & cardinality] vattr
                      cardinality (if (= :many (first cardinality)) :db.cardinality/many :db.cardinality/one)]]
            {:db/id #db/id [:db.part/db]
             :db/ident (keyword (str (name namespace) "/" (name attr)))
             :db/valueType (keyword (str "db.type/" (name type)))
             :db/cardinality cardinality
             :db.install/_attribute :db.part/db})))
   (flatten)
   (vec)))

(comment
  [{:db/id #db/id [:db.part/db]
    :db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "A person's name"
    :db.install/_attribute :db.part/db}]


  ;; http://docs.datomic.com/clojure-api.html
  (def uri "datomic:mem://test")
  (defn reset-db []
    (if-let [conn (d/connect uri)]
      (d/delete-database uri))
    (d/create-database uri)
    (d/connect uri))
  
  (defn reconn []
    (d/connect uri))
  
  
  )
