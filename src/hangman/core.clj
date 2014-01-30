(ns hangman.core
  (:require
    [clojure.string  :as str]
  ) )


(defn show-info 
  "Print synopsis info about a sequence of strings"
  [ seqVals seqName ]
  (println 
    (str seqName "("  (count seqVals) ") " )
      (take 5 (map #(str \" % \") seqVals) ) ))

(defn words-of-len
  "Finds words from a sequence with the specified length "
  [word-seq target-len]
  (filter #(= target-len (count %)) word-seq) )

(def all-words   
  (->> (slurp"resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(defn main []
  (println "main: enter")
  (show-info all-words "all-words")
  (let [ 
    max-len (apply max (map #(count %) all-words) ) 
    map-by-size  (group-by count all-words)
    ] 
    (println "map-by-size:")
    (doseq [ curr-len (range 1 (inc max-len)) ]
      (let [ curr-words   (get map-by-size curr-len [] ) ]
        (show-info curr-words (str "len=" curr-len) ) )
    )
  ))
