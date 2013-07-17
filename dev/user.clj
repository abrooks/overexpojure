(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [clojure.repl :refer :all]
            [overexpojure.core :as oec]
            [overexpojure.datomic :as oed]
            [datomic.api :as d :refer [db q]]))

(defn create
  "Creates and stores a new datomic mem-db instance."
  []
  (d/create-database oed/uri))

(defn stop
  "Shuts down the application and releases resources."
  []
  (d/delete-database oed/uri))

(defn reset []
  (stop)
  (refresh :after 'user/create))
