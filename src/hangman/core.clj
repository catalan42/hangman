(ns hangman.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
  ) )


(def show-info-size 10)

(defn show-info 
  "Print synopsis info about a sequence of strings"
  [ seqVals seqName ]
  (println 
    (str seqName "("  (count seqVals) ") " )
        (take show-info-size (map #(str \" % \") seqVals)) ))

(def all-chars 
  "A set of all lowercase characters [a..z]"
  (->> (range  (int \a)  (inc (int \z))  )
       (map char)
        set ))

(def all-words   
  "A collection of all words for the hangman game."
  (->> (slurp "resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(def tst-words [ 
  "is" "at" "by" "up"
  "act" "add" "ace" "and" "ask" "abe" "ads"
  "bad" "bug" "bed" "bet"  "big" "bit"
  "cat" "car" "can" "cob" "con" "cop" "cup" 
  "work" "love" "hate" "jobs" "able" "ball" ] )

(defn words-by-length  
  "Maps word length to word values.  Each key is a word length. Each value is a collection
  of all words of that length.  Keys are absent if no words of that length are present."
  [word-seq]
  (group-by count word-seq) )

(defn to-word-array 
  "Returns all words of the specified length as a 2D array (vector of vectors).
  First index selects a given word, 2nd index selects chars from that word."
  [word-seq]
  { :pre  []
    :post [ (vector? %) ] }
  (vec
    (map vec word-seq) ))

(defn num-rows
  "Given a 2D array (vector of vectors), return the number of rows (1st dimension)."
  [word-array]
  { :pre  [ (vector? word-array) ]
    :post [] }
  (count word-array) )

(defn num-cols
  "Given a 2D array (vector of vectors), return the number of columns (2nd dimension)."
  [word-array]
  { :pre  [ ;(vector?  word-array) 
            ;(vector? (word-array 0)) 
            ]
    :post [] }
  (count (word-array 0)) )

(defn get-array-col
  "Given a 2D array (vector of vectors), return a vector of elements 
  from the specified column."
  [word-array col-idx]
  { :pre  [ (vector? word-array) (integer? col-idx) ]
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

(defn freqs-by-col
  "For each column of a 2D array, computes an element frequency map. Returns as 
  a vector indexed by column."
  [word-array]
  { :pre  [ (vector? word-array)
            (< 0 (num-rows word-array) )
            (< 0 (num-cols word-array) ) ] 
    :post [ (vector? %) ] }
  (vec
    (for [idx (range 0 (num-cols word-array)) ]
      (let [curr-col (get-array-col word-array idx)]
        (frequencies curr-col)) )))

(defn filter-with-idx
  "Returns values from data-seq where corresponding pred-seq elements are truthy.
  An indexed-based implementation."
  [pred-seq data-seq]
  { :pre  [ (= (count pred-seq) (count data-seq)) ] 
    :post [ (vector? %) ] }
  (let [data-vec  (vec data-seq)
        pred-vec  (vec pred-seq)
        filt-fn   (fn [idx data-val] 
                    (if (pred-vec idx) (data-vec idx) ))
        filt-seq  (keep-indexed filt-fn data-vec) 
  ] (vec filt-seq) ))

(defn filter-with-seq
  "Returns values from data-seq where corresponding pred-seq elements are truthy.
  A sequence-based implementation"
  [pred-seq data-seq]
  (let [ pair-seq     (map vector pred-seq data-seq )
         filt-seq     (filter #(% 0) pair-seq)
         filt-data    (map second filt-seq) 
  ] (vec filt-data) ))

(defn filter-with 
  "Returns values from data-seq where corresponding pred-seq elements are truthy."
  [pred-seq data-seq]
  { :pre  [ (= (count pred-seq) (count data-seq)) ] 
    :post [ (vector? %) ] }
  (filter-with-seq pred-seq data-seq) )

(defn make-guess
  "Generate the next guess char. clue-vec consists of chars or nil, where a char shows
  correctly guessed letters, and nil shows chars not yet guessed.  "
  [clue-vec col-char-freqs used-chars]
  (let [
    all-char-freqs  (apply merge-with + col-char-freqs)
    avail-chars     (set/difference (set(keys all-char-freqs)) used-chars)
    max-avail-char  (apply max-key all-char-freqs avail-chars ) 
  ] max-avail-char ) )

(defn make-clue
  "Generate a clue given the target word and a vec of guessed letters."
  [target-vec guesses-set]
  (vec 
    (for [ letter target-vec ] (guesses-set letter)) ))

(defn run-tests []
  (println "----------------------------------------")
  (println "Tests:")

  ; Filtering one sequence with another
  (let [
    pred-vals5  [ true false 5 nil :a ] 
    pred-vals8  [ true false true nil true nil false true ] 
  ]
    (assert (= (filter-with     pred-vals5 (range 5)) [0 2 4  ] ))
    (assert (= (filter-with     pred-vals8 (range 8)) [0 2 4 7] )) 
    (assert (= (filter-with-seq pred-vals8 (range 5)) [0 2 4  ] ))
    (assert (= (filter-with-seq pred-vals8 (range 8)) [0 2 4 7] )) 
    )

  ; Manipulation of strings/vectors/character seq's
  (assert (= (vec "abcd")                [\a \b \c \d] ))
  (assert (= (str/join  (vec "abcd"))    "abcd" ))
  (assert (= (apply str (vec "abcd"))    "abcd" ))

  ; Test array slicing functions
  (let [ tstArr [ [:a :b :c] [1 2 3] [\a \b \c]] ]
    (assert (= (get-array-row tstArr 0) [:a :b :c] ))
    (assert (= (get-array-row tstArr 1) [ 1  2  3] ))
    (assert (= (get-array-row tstArr 2) [\a \b \c] ))
    (assert (= (get-array-col tstArr 0) [:a  1 \a] ))
    (assert (= (get-array-col tstArr 1) [:b  2 \b] ))
    (assert (= (get-array-col tstArr 2) [:c  3 \c] ))
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

  ; Test frequency function
  (let [
    tst-words [ "abcd" "xbcd" "xxcd" "xxxd" ] 
    words-map           (words-by-length tst-words)
      _ (assert (= words-map   {4 ["abcd" "xbcd" "xxcd" "xxxd"]} ))

    curr-len            4
    curr-words          (words-map curr-len)
      _ (assert (= curr-words     ["abcd" "xbcd" "xxcd" "xxxd"]  ))
      _ (println "curr-words:" curr-words)
    word-array          (to-word-array  curr-words)
    col-char-freqs      (freqs-by-col word-array)
      _ (assert (= col-char-freqs [{\a 1, \x 3} {\b 2, \x 2} {\c 3, \x 1} {\d 4}] ))

    all-char-freqs      (apply merge-with + col-char-freqs)
      _ (assert (= all-char-freqs { \a 1, \b 2, \c 3, \d 4, \x 6} )) 

    max-freq-val        (apply max-key all-char-freqs (keys all-char-freqs) )
      _ (assert (= max-freq-val \x ))

    tgt-word         [ \x  \b  \c  \d  ]
    old-clue            [ nil \b  nil nil ]

    keepFlg             (map #(nil? %) old-clue )   
      _ (println "keepFlg" keepFlg)
    keepFreqs           (filter-with keepFlg col-char-freqs)
      _ (println "keepFreqs" keepFreqs)
    keepWords           (filter-with keepFlg curr-words)
      _ (println "keepWords" keepWords)

    guessed-chars       #{ \b }
      _ (println "guessed-chars" guessed-chars)
    new-guess           (make-guess old-clue col-char-freqs guessed-chars)
      _ (println "new-guess" new-guess)
    guessed-chars       (conj guessed-chars new-guess)
      _ (println "guessed-chars" guessed-chars)

    new-clue            (make-clue tgt-word guessed-chars)
      _ (println "tgt-word" tgt-word)
      _ (println "old-clue" old-clue)
      _ (println "new-clue" new-clue)

  ]
  )
)

(defn main 
  ( [] (main tst-words) )
  ( [word-seq]
      (run-tests)
      (println "----------------------------------------")
      (let [words-map     (words-by-length word-seq)
            max-word-len  (apply max (keys words-map) )
            ]
        (show-info word-seq "ALL")
        (doseq [ curr-len (sort (keys words-map)) ]
          (println)
          (let [ curr-words  (words-map curr-len)
                 word-array  (to-word-array  curr-words)
                 _ (show-info curr-words (str "len=" curr-len) )
                 col-char-freqs (freqs-by-col word-array)
                 _ (println "freqs-by-col:" col-char-freqs)
                 all-char-freqs (apply merge-with + col-char-freqs)
                 _ (println "all-char-freqs" all-char-freqs)
          ] )
        )
      )
  )
)
(defn -main [& args] (apply main args) )

; (defonce sanity-check (run-tests) )
