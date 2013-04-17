(ns overexpojure.core
  (require [pl.danieljanus.tagsoup :as ts]
           [clojure.walk :refer [prewalk]]
           [clj-time.core :as time]
           [clj-time.format :as tf]))

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
  (loop [out [] in tr]
    (if (empty? in)
      out
      (let [[f & r] in
            span (Integer. (get-attr f :colspan "1"))]
        (if (< 1 span)
          (recur (into out (repeat span f)) (drop (dec span) r))
          (recur (conj out f) r))))))

;; Convert the below to clj-time format?
(def time-pattern #"^\d\d?:\d\d [AP]M$")

;;; cf. http://joda-time.sourceforge.net/apidocs/org/joda/time/format/DateTimeFormat.html
(def day-parser (tf/formatter (time/default-time-zone) "EEE" "EEEE"))
(def date-parser (tf/formatter (time/default-time-zone) "MMM-dd" "MMMM-dd"))

(defmacro catch-false [& body]
  "Oh, the terrible things Java makes us do..."
  `(try
    ~@body
    (catch Throwable t#
      false)))

(defn categorize [tr]
  (let [item (first (flatten tr))]
    (cond
     (re-find time-pattern item) :time
     (catch-false (tf/parse day-parser item)) :day
     (catch-false (tf/parse date-parser item)) :date
     :else :room)))

(comment
  ;; Starts with :html
  ;; How-to-run comments here
  (html-> e :body :table :tbody)
  (html-> e :body :table :tbody :tr :td)

  (binding [*print-meta* false]
    (doseq [tr (html-> (un-html f) :body :table :tbody)]
      (prn (ex-span-d tr))))

  (group-by categorize (for [tr (rest (html-> (un-html f) :body :table :tbody))]
                         (ex-span-d (rest tr))))

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
