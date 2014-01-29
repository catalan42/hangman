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
         max-len (apply max (map #(count %) words) ) 
         _  (println "max-len:" max-len)
         _  (doall 
              (for [ curr-len (range 1 (inc max-len)) ]
                (let [ curr-words (find-words-len words curr-len) ]
                  (show-info curr-words 
                   (str "len=" curr-len) ) )))
       ] 
  )
)
