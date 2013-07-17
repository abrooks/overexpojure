(ns overexpojure.datomic
  (require [datomic.api :as d :refer [db q]]))

;; TODO: how to handle DB connection?
(def uri "datomic:mem://test")

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
            ;; TODO: #db/id is broken here. Every attribute gets the same ID.
            ;; Surely I'm doing this wrong.
            {:db/id #db/id [:db.part/db]
             :db/ident (keyword (str (name namespace) "/" (name attr)))
             :db/valueType (keyword (str "db.type/" (name type)))
             :db/cardinality cardinality
             :db.install/_attribute :db.part/db})))
   (flatten)
   (vec)))




(comment
  ;; to test:
  (reset)
  @(d/transact (d/connect oed/uri) oed/schema)
  @(d/transact (d/connect oed/uri) [{:db/id #db/id[:db.part/user -1]
                                     :conference/name "hamish"
                                     :conference/location "canada"
                                     :conference/year 1996
                                     :conference/tags ["gordon korman"]}])
  (def e (-> (d/connect oed/uri)
             db
             (d/entity
              (ffirst
               (q '[:find ?e
                    :where [?e :conference/location]]
                  (db (d/connect oed/uri)))))))


  
  [{:db/id #db/id [:db.part/db]
    :db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "A person's name"
    :db.install/_attribute :db.part/db}]


  ;; http://docs.datomic.com/clojure-api.html
  (defn reset-db []
    (if-let [conn (d/connect uri)]
      (d/delete-database uri))
    (d/create-database uri)
    (d/connect uri))
  
  (defn reconn []
    (d/connect uri))
  


  @(d/transact tconn [{:db/id #db/id [:db.part/db] :db/ident :person/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db.install/_attribute :db.part/db}])

  @(d/transact tconn [{:db/id #db/id[:db.part/user -1] :person/name "hamish"}])
  
  (def results (q '[:find ?e :where [?e :person/name]] (db tconn)))

  (def id (ffirst results))
  (def entity (-> conn db (d/entity id)))

  ;; display the entity map's keys
  (keys entity)
  
  )
