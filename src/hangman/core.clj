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

(defn find-words-len
  "Finds words of a certain length from a sequence"
  [word-seq target-len]
  (filter #(= target-len (count %)) word-seq) )

(defn main []
  (println "Hello, World!")
  (let [ words   (->> (slurp"resources/words.txt")
                      (str/split-lines )
                      (map str/trim ) ) 
         _       (show-info words "words")
         len1    (find-words-len words 1 ) 
         _       (show-info len1 "len1")
         len2    (find-words-len words 2 ) 
         _       (show-info len2 "len2")
         len3    (find-words-len words 3 ) 
         _       (show-info len3 "len3")
         max-len (apply max (map #(count %) words) ) 
         _  (println "max-len:" max-len)
       ] 
  )
)
