(ns overexpojure.core
  (require [pl.danieljanus.tagsoup :as ts]
           [clojure.walk :refer [prewalk]]))

(def e (ts/parse "file:ClojureConj2012.html"))
(def f (ts/parse "file:ClojureWest2012.html"))

(defn meta-ize [[tag attr & cont]]
  (with-meta
    (into [] cont)
    {:tag tag :attr attr}))

(defn un-html [html]
  (prewalk #(if (vector? %)
              (meta-ize %)
              %)
           html))

(defn get-tag [e]
  (:tag (meta e)))

(defn get-attrs [e]
  (:attr (meta e)))

(defn get-attr
  ([e k] (get-attr e k nil))
  ([e k d]
     (get (get-attrs e) k d)))

(defn html->
  ([html] html)
  ([html fpath & rpath]
     (apply html->
            (first (filter #(= fpath (get-tag %)) html))
            rpath)))

(defn ex-span-d [tr]

#_(binding [*print-meta* true]
    (doseq [tr (html-> (un-html f) :body :table :tbody)
            td tr
            :when (= "3" (get-attr td :colspan))]
      (prn td)))

  )


(comment
  ;; Starts with :html
  ;; How-to-run comments here
  (html-> e :body :table :tbody)
  (html-> e :body :table :tbody :tr :td)

  {:conferences {:conj-2010
                 {:name "Clojure/conj 2010"
                  :location "Raleigh, NC"
                  :attendees 150
                  :rooms {:main "Main room"}
                  :tags #{:clojure-conj}}
                 :west-2012
                 {:name "Clojure/West 2012"
                  :location "San Jose, CA"}}
   :speakers {:fogus {:name "Michael Fogus"
                      :bio "He's just this guy, you know..."
                      :links #{:twitter "http://twitter.com/fogus"
                               :github "https://github.com/fogus"}}}
   :talks [{:conference :conj-2010
            :when #inst "2010-10-22T09:40"
            :room :main
            :duration 18
            :speakers #{:fogus}
            :title "Fertile Ground: The Roots of Clojure"
            :links {:video "http://www.youtube.com/watch?v=NnSpaR67hXg"}
            :tags #{:history}}]}
  )
