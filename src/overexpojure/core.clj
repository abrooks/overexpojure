(ns overexpojure.core
  (require [pl.danieljanus.tagsoup :as ts]
           [clojure.walk :refer [prewalk]]
           [clj-time.core :as time]
           [clj-time.format :as tf]))

(def c2010 (ts/parse "file:ClojureConj2010.html"))
(def c2011 (ts/parse "file:ClojureConj2011.html"))
(def c2012 (ts/parse "file:ClojureConj2012.html"))
(def w2012 (ts/parse "file:ClojureWest2012.html"))
(def allconfs
  [[c2010 "2010" "Clojure/conj"]
   [c2011 "2011" "Clojure/conj"]
   [c2012 "2012" "Clojure/conj"]
   [w2012 "2012" "Clojure/west"]])
(def talksdata (flatten (map #(apply data-ify %) allconfs)))

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
(def when-parser (tf/formatter (time/default-time-zone)
                               "YYYY-MMM-dd h:mm aa zzz"
                               "YYYY-MMM-dd hh:mm aa zzz"
                               "YYYY-MMMM-dd h:mm aa zzz"
                               "YYYY-MMMM-dd hh:mm aa zzz"))

(defn catch-false
  "Oh, the terrible things Java makes us do..."
  [f]
  (try
    (f)
    (catch Throwable t
      false)))

(defn tr-categorize
  "Categorize the tr as :time, :day, or :date"
  [tr]
  (let [item (first (flatten tr))
        item (or item "")]
    (cond
     (re-find time-pattern item) :time
     (catch-false #(tf/parse day-parser item)) :day
     (catch-false #(tf/parse date-parser item)) :date
     (not (empty? item)) :room
     :else :skip)))

(defn categorize
  [html]
  (let [uh (html-> (un-html html) :body :table :tbody)]
    (group-by tr-categorize (for [tr (rest uh)]
                              (ex-span-d (rest tr))))))

(defn talk-ify [{:keys [conf year date tslot room talk]}]
  (let [spkrs-title (ffirst talk)
        [_ spkrs-str title] (re-matches #"([^/]*) / (.*)" spkrs-title)
        spkrs-str (or spkrs-str "")
        title (or title spkrs-title)
        spkrs (map (memfn trim) (re-seq #"[^,&]+" spkrs-str))
        when (->> (str year "-" date " " tslot " UT")
                  (tf/parse when-parser)
                  .toDate)]
    {:conference conf
     :when when
     :room room
     :speakers spkrs
     :title title
     :links {:video (get-attr (first talk) :href "")}}))

(defn data-ify
  "Convert HTML into our special data format"
  [html year conf]
  (let [{:keys [date room time]} (categorize html)
        date (-> date first next first)
        room (-> room first next first)
        room (or room [""])]
    (for [t time
          [date room talk] (map vector date room (rest t))
          :let [tslot (ffirst t)
                talk (if (-> talk first string?)
                       [talk]
                       talk)]
          :when (ffirst talk)]
      (talk-ify {:conf conf, :year year
                 :date date, :tslot tslot
                 :room room, :talk talk}))))

(defn collect-val [dataset key talk]
  (prn dataset key talk)
  (if (empty? (talk key))
    dataset
    (conj dataset (talk key))))

(defn coalesce-confs [talksdata]
  (reduce #({:conferences (collect-val (% :conferences) :conference %2)
             :speakers (collect-val (% :speakers) :speakers %2)
             :talks (conj (% :talks) %2)})
          {:conferences #{} :speakers #{} :talks []}
          talksdata))

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

  (data-ify w2012 "2012" "Clojure/West")

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

  {:day [[[] ["Friday"] ["Friday"] ["Friday"] [] ["Saturday"] ["Saturday"] ["Saturday"]]],
   :date [[[] ["Mar-16"] ["Mar-16"] ["Mar-16"] [] ["Mar-17"] ["Mar-17"] ["Mar-17"]]],
   :room [[[] ["Salon 1-4"] ["Salon 5-6"] ["Willow Glen"] [] ["Salon 1-4"] ["Salon 5-6"] ["Willow Glen"]]],
   :time [[["9:00 AM"] [["Rich Hickey / The Design of Datomic"]] [] [] [] [["Bradford Cross / Why Prismatic Goes Faster with Clojure"]] [] []]
          [["10:00 AM"] [["David McNeil / Macros are Hard!"]] [["Craig Andera / Namespaces, Vars, and Symbols (Oh My!)"]] [["Reid Draper / Knockbox, an Eventual Consistency Toolkit"]] [] [["Nathan Marz / Storm: Distributed and Fault-tolerant Real-time Computation"]] [["Zach Tellman / Distilling Java Libraries"]] [["Allen Rohner / The Good, The Bad & The Ugly (Clojure and JRuby)"]]]
          [["11:00 AM"] [["Alan Dipert / Programming with Values in Clojure"]] [["Amit Rathore / clojure @ runa :: dynamic pricing through DSLs"]] ["Colin Jones / Clojure Koans Hackfest"] [] [["Chris Houser / Distributed Apps: The Joys of Testing and Debugging"]] [["Colin Jones / SOLID Clojure"]] ["Federico Brubacher / Real world Cascalog: Past, Present, Future"]]
          [["1:00 PM"] [["Chas Emerick /  What Sucks about Clojure... and Why You'll Love It Anyway"]] [["Baishampayan Ghose / The Taming of the Deftype"]] [["Jim Crossley / Introducing Immutant"]] [] [] [] []]
          [["2:00 PM"] ["Andy Kringer / Load testing with Clojure"] [["Antoni Batchelli / Pallet - DevOps for the JVM"]] [["Dave Ray / Building User Interfaces with Seasaw"]] [] [["Sean Corfield / Real World Clojure - Doing Boring Stuff with an Exciting Language"]] [["Kevin Lynagh / Statistical Graphics, ClojureScript, &c."]] [["Phil Hagelberg / Swarm Coding"]]]
          [["2:30 PM"] [["Aaron Bedra / The Generative Generation"]] [["Alan Witaker / Building tools to help kids Win with ADHD"]] [["Micah Martin / Clojure in the Clouds"]] [] [["Daniel Solano Gómez / Crunching Numbers with Clojure - 11 Tips to Boost Your Performance"]] [["Luke VanderHart / Beyond Ninjas: DOM manipulation with ClojureScript and Domina"]] []]
          [["3:00 PM"] [["Bill Caputo / Continuous Testing in Clojure"]] [["Paul deGrandis / Clojure-powered Startups"]] [["Pat Patterson / Accessing Real-World APIs from Clojure"]] [] [["Jim Duey / DSLs in Clojure"]] [["Creighton Kirkendall / Building ClojureScript Libraries: Google Closure and Challenges of a Young Language"]] []] [["4:00 PM"] [["Stuart Sierra / Thinking in Data"]] [["Tyler Jennings / Bootstrapping Clojure at Groupon"]] [["Carin Meier / Why is a Monad Like a Writing Desk"]] [] [["Michael Fogus / ClojureScript Anatomy"]] [["Ryan Senior / Practical core.logic"]] [["Paul Stadig / Laziness: the Good, the Bad and the Ugly"]]] [["5:00 PM"] [["Richard Gabriel / Engineering(,) A Path to Science: 'I don't want to die in a language I can't understand'"]] [] [] [] [["Stuart Halloway / Evident Code, at Scale"]] [] []] [["7:00 PM"] [] ["Unsession: Hacking with Clojure on Android"] [] [] [] [] []] [["7:30 PM"] ["Unsession: Clojure User Groups - creating and running them"] ["Unsession: Infrastructure automation with Pallet"] ["Unsession: Pino and ClojureScript apps"] [] [] [] []] [["8:00 PM"] ["Unsession: Clojurepunk for the Masses"] ["Unsession: Hacking with Datomic"] [] [] [] [] []] [["9:00 PM"] ["Unjam: Release your inner Clojurepunk"] ["Unsession: ClojureScript and Testing"] [] [] [] [] []]]}

  )
