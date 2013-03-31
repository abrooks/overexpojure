(ns overexpojure.core
  (require [pl.danieljanus.tagsoup :as ts]))

(def e (ts/parse "file:///home/moquist/projects/overexpojure/ClojureConj2012.html"))

(defn find-first [[tag attr & content] name]
  (some #(when (= name (first %))
           %)
        content))

(defn html->
  ([html] html)
  ([html fpath & rpath]
     (apply html-> (find-first html fpath) rpath)))

(comment
  ;; Starts with :html
  ;; How-to-run comments here
  (html-> e :body :table :tbody)
  (html-> e :body :table :tbody :tr :td)
)
