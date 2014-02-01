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

(def words-by-length  
  "Maps word length to word values.  Each key is a word length. Each value is a collection
  of all words of that length.  Keys are absent if no words of that length are present."
  (group-by count all-words) )

(def max-word-length 
  "The maximum length of any word."
  (apply max (keys words-by-length)) )

(defn to-word-array 
  "Returns all words of the specified length as a 2D array (vector of vectors).
  First index selects a given word, 2nd index selects chars from that word."
  [word-length]
  { :pre  [ (integer? word-length) ]
    :post [ (vector? %) ] }
  (vec
    (map vec (words-by-length word-length)) ))

(defn get-array-column
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified column."
  [word-array col-idx]
  { :pre  [ (integer? col-idx) ]
    :post [ (vector? %) ] }
  (reduce #(conj %1 (%2 col-idx))
          [] word-array ) )

(defn get-array-row
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified row."
  [word-array row-idx]
  { :pre  [ (integer? row-idx) ]
    :post [ (vector? %) ] }
  (word-array row-idx) )

(defn run-tests []
  ; Manipulation of strings/vectors/character seq's
  (assert (= (vec "abcd")                [\a \b \c \d] ))
  (assert (= (str/join  (vec "abcd"))    "abcd" ))
  (assert (= (apply str (vec "abcd"))    "abcd" ))

  (let [ tstArr [ [:a :b :c] [1 2 3] [\a \b \c]] ]
    (assert (= (get-array-row    tstArr 0) [:a :b :c] ))
    (assert (= (get-array-row    tstArr 1) [ 1  2  3] ))
    (assert (= (get-array-row    tstArr 2) [\a \b \c] ))
    (assert (= (get-array-column tstArr 0) [:a  1 \a] ))
    (assert (= (get-array-column tstArr 1) [:b  2 \b] ))
    (assert (= (get-array-column tstArr 2) [:c  3 \c] ))
  )

  ; Use of (frequencies...) and (merge-with...)
  (assert (= (frequencies "abbccc")          
             {\a 1, \b 2, \c 3} ))
  (assert (= (frequencies ["abc" "bc" "c"])  
             {"abc" 1 "bc" 1 "c" 1} ))
  (assert (= (map frequencies ["abc" "bc" "c"])
             [ {\a 1, \b 1, \c 1} {\b 1, \c 1} {\c 1} ] ))
  (assert (= (apply merge-with + (map frequencies ["abc" "bc" "c"]) )
             {\a 1 \b 2 \c 3} ))
)

(defn words-of-length 
  "Returns a seq of words of the specified length."
  [word-length]
  { :pre  [ (integer? word-length) ]
    :post [ ] }
  (get words-by-length word-length [] ) )

; (def data-map
;   "A map of data keyed by word length"
;   (doseq [curr-len (range 4 (inc 4)) ]
;     (apply merge-with + 
;       (map frequencies (words-of-length curr-len)) )))

(defn main []
  (println "main: enter")
  (run-tests)
  (show-info all-words "all-words")
  (doseq [ curr-len    (range 1 (inc max-word-length)) ]
    (show-info (words-of-length curr-len) (str "len=" curr-len) ) )
)

(defonce sanity-check (run-tests) )

