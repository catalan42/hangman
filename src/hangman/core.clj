(ns hangman.core
  (:require
    [clojure.string  :as str]
    [clojure.set     :as set]
  ) )

;----------------------------------------------------------------------
; Simple logging tools for demo. Replace in production
;
(def ^:const LOG-LEVEL-ERROR    5 )
(def ^:const LOG-LEVEL-WARN     4 )
(def ^:const LOG-LEVEL-NORMAL   3 )
(def ^:const LOG-LEVEL-EXTRA    2 )
(def ^:const LOG-LEVEL-DBG      1 )

(def logging-enabled           true )
(def logging-min-level         LOG-LEVEL-NORMAL )

(defn write-to-log
  "Write log msg to console for debugging."
  [ level & msgs]
  (when (and logging-enabled
             (<= logging-min-level level) )
    (apply println msgs ) ))

; Convenience functions
(defn log-err   [& msgs] (apply write-to-log  LOG-LEVEL-ERROR    msgs ))
(defn log-warn  [& msgs] (apply write-to-log  LOG-LEVEL-WARN     msgs ))
(defn log-msg   [& msgs] (apply write-to-log  LOG-LEVEL-NORMAL   msgs ))
(defn log-extra [& msgs] (apply write-to-log  LOG-LEVEL-EXTRA    msgs ))
(defn log-dbg   [& msgs] (apply write-to-log  LOG-LEVEL-DBG      msgs ))

;----------------------------------------------------------------------
;
(def ^:const all-letters (set (map char (range (int \a) (inc(int \z)) ))) )

(def all-words   
  "A collection of all words for the hangman game."
  (->> (slurp "resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(def  show-info-size 8)
(defn show-info
  "Print synopsis info about a sequence of strings"
  [ seqName seqVals ]
  (log-extra 
    (str seqName "("  (count seqVals) ") " )
        (take show-info-size (map #(str \" % \") seqVals)) ))

(defn make-clue
  "Generate a clue given the target word and a vec of guessed letters. The clue is a
  vector the length of the target word consisting of either letters (for correctly guessed
  letters) or nil (for incorrect guesses)."
  [tgt-word guessed-chars]
  (str/join (for [ letter tgt-word ] (get guessed-chars letter \- ))) )

(defn complement-char-class
  "Generate the regex for the complement of a set of characters."
  [not-chars]
  (if (< 0 (count not-chars))
    (str "[^" (str/join not-chars) "]" )  ;normal case
    "."    ; degenerate case of no chars guessed yet - anything matches
  ))

(defn clue-to-regex
  "Convert a clue string and set of guessed chars into a regex for filtering possible
  matching words."
  [clue-str guessed-chars]
  (let [not-guessed-class   (complement-char-class guessed-chars)
        patt-str   (str/join
                     (for [clue-ch clue-str]
                       (if (= clue-ch \-) not-guessed-class clue-ch) ))
    ] patt-str ))

(defn filter-words 
  "Filter words retaining only those that are possible matches given the current clue and
  already guessed chars."
  [clue guessed-chars words]
  (let [patt-str      (clue-to-regex clue guessed-chars) 
        re-patt       (re-pattern patt-str)
        keep-words    (filter #(re-find re-patt %) words ) ]
    keep-words ))

(defn make-guess
  "Choose the most common letter. Same idea as building a Huffman code."
  [words used-chars]
  (let [avail-chars   (set/difference all-letters used-chars)
        char-freqs    (frequencies (str/join words))
        best-char     (apply max-key #(get char-freqs % -1) avail-chars) ]
    best-char ))

(defn do-tests []
  ; Test regex stuff
  (assert (= (complement-char-class #{})         "."      ))
  (assert (= (complement-char-class #{\a \b})    "[^ab]"  ))
  (let [words           ["abcd" "xbcd" "xxcd" "xxxd"]
        letters         [\a \b \c \d] 
        guessed-chars   #{\b \s}
        clue-str        "-b--"
        not-char-class  (complement-char-class    guessed-chars)
        patt-str        (clue-to-regex  clue-str  guessed-chars)
  ]
    (assert (= not-char-class  "[^bs]"                 ))
    (assert (= patt-str        "[^bs]b[^bs][^bs]"      ))
    (assert      (re-find (re-pattern patt-str) "abcd" ))
    (assert      (re-find (re-pattern patt-str) "xbcd" ))
    (assert (not (re-find (re-pattern patt-str) "xxcd" )))
    (assert (not (re-find (re-pattern patt-str) "xxxd" )))

    (assert (= (filter-words "----"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "abcd"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "a---"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "ab--"  #{}      ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "-b--"  #{\b \s} ["abcd"] ) ["abcd"] ))
    (assert (= (filter-words "a---"  #{\b \s} ["abcd"] ) []       ))
  )
)

(defonce test-results (do-tests) )

(defn play-hangman 
  "Plays the hangman game for the supplied word.  Returns the number of letter guesses
  required to find a unique match in the master words list."
  [tgt-word]
  (let [game-words    all-words
        words-map     (group-by count game-words)  ; map keyed by word length
        word-list     (words-map (count tgt-word)) ; words of correct length
    ]
    (loop [ guessed-chars   #{}
            clue            (make-clue tgt-word guessed-chars) ]
      (log-extra )
      (let [ keep-words (filter-words clue guessed-chars word-list) ]
        (show-info "keep-words" keep-words)
        (if (= 1 (count keep-words))
          (let [final-guess         (str/join (first keep-words)) 
                num-letter-guesses  (count guessed-chars) ]
            (log-extra (str "***** found word:  '" final-guess 
                        "'   Guesses:  " (count guessed-chars) "  *****") )
            num-letter-guesses ; return value
          )
        ;else
          (let [new-guess       (make-guess keep-words guessed-chars)
                guessed-chars   (conj guessed-chars new-guess)
                new-clue        (make-clue tgt-word guessed-chars) ]
            (log-extra "clue: " clue "  new-guess" new-guess 
              "  guessed-chars (" (count guessed-chars) ")" guessed-chars)
            (recur  (conj guessed-chars new-guess)  new-clue )
          )
        ) ))))

(def baseline-scores {
      "comaker" 25 "cumulate" 9 "eruptive" 5 "factual" 9 "monadism" 8 
      "mus" 25 "nagging" 7 "oses" 5 "remembered" 5 "spodumenes" 4 
      "stereoisomers" 2 "toxics" 11 "trichromats" 5 "triose" 5 "uniformed" 5 } )


(defn main 
  [] 
  (doseq [ tgt-word (keys baseline-scores) ]
    (let [ num-letter-guesses   (play-hangman tgt-word) ]
      (log-msg (str   "word:  " (format "%-15s" tgt-word) 
                 "  guesses:  " (format "%2s" num-letter-guesses)
                "  baseline:  " (format "%2s" (baseline-scores tgt-word)) ))))
)

(defn -main [& args] (apply main args) )

