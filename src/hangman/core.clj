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

(defn main []
  (println "Hello, World!")
  (let [ 
     words   (->> (slurp"resources/words.txt")
                  (str/split-lines )
                  (map str/trim ) )
     _       (show-info words "words")
     max-len (apply max (map #(count %) words) ) 
     _  (println "max-len:" max-len)
     _  (do 
          (println "for loop:")
          (doall (for [ curr-len (range 1 (inc max-len)) ]
            (let [ curr-words (words-of-len words curr-len) ]
              (show-info curr-words (str "len=" curr-len) )
            )) )
          (println "leaving doall #1")
          )
     map-by-size  (group-by count words)
     _   (println "map-by-size:")
     _   (doall (for [ curr-len (range 1 (inc max-len)) ]
             (let [ curr-words   (get map-by-size curr-len [] ) ]
              (show-info curr-words (str "len=" curr-len) ) )
           ))
     ] 
  )
)
