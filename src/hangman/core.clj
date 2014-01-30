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

(def all-words   
  "A collection of all words for the hangman game."
  (->> (slurp"resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(def len-word-map  
  "Each key is a word length. Each value is a collection of all words of that length. Some
  keys may be absent if no words of that length are present."
  (group-by count all-words) )

(def max-len 
  "The maximum length of any word."
  (apply max (keys len-word-map)) ) 

(defn main []
  (println "main: enter")
  (show-info all-words "all-words")
  (doseq [ curr-len (range 1 (inc max-len)) ]
    (let [ curr-words   (get len-word-map curr-len [] ) ]
      (show-info curr-words (str "len=" curr-len) ) )
  ) )
