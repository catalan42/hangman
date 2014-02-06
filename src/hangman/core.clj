(ns hangman.core
  (:require
    [clojure.string          :as str]
    [clojure.set             :as set]
  ) 
  ; Import classes from the Java "default" package.  
  (:import GuessingStrategy  Guess  GuessLetter  GuessWord  
           HangmanGame HangmanUtils )
    ;********************************************************************************
    ; NOTE:  For some reason, class HangmanGame was not declared public (originally it had
    ; "package" access), despite all other classes/interfaces being public.  This caused a
    ; java.lang.IllegalAccessException during execution!!!  This looks like a bug in the
    ; demo code.
    ;********************************************************************************
)

;----------------------------------------------------------------------
; Simple logging tools for demo. Replace with log4j or similar in production.
;
(def ^:const LOG-LEVEL-ERROR    5 )
(def ^:const LOG-LEVEL-WARN     4 )
(def ^:const LOG-LEVEL-NORMAL   3 )
(def ^:const LOG-LEVEL-EXTRA    2 )
(def ^:const LOG-LEVEL-DEBUG    1 )

(def logging-enabled           true )
(def logging-min-level         LOG-LEVEL-NORMAL )

(defn write-to-log
  "Write log msg to console for debugging."
  [ level & msgs]
  (when (and logging-enabled
             (<= logging-min-level level) )
    (apply println msgs ) ))

; Convenience functions
(defn log-error  [& msgs] (apply write-to-log  LOG-LEVEL-ERROR    msgs ))
(defn log-warn   [& msgs] (apply write-to-log  LOG-LEVEL-WARN     msgs ))
(defn log-msg    [& msgs] (apply write-to-log  LOG-LEVEL-NORMAL   msgs ))
(defn log-extra  [& msgs] (apply write-to-log  LOG-LEVEL-EXTRA    msgs ))
(defn log-dbg    [& msgs] (apply write-to-log  LOG-LEVEL-DEBUG    msgs ))

; End logging tools
;----------------------------------------------------------------------

(def ^:const all-letters (set (map char (range (int \a) (inc(int \z)) ))) )

(def baseline-scores {
      "comaker" 25 "cumulate" 9 "eruptive" 5 "factual" 9 "monadism" 8 
      "mus" 25 "nagging" 7 "oses" 5 "remembered" 5 "spodumenes" 4 
      "stereoisomers" 2 "toxics" 11 "trichromats" 5 "triose" 5 "uniformed" 5 } )

(def all-words   
  "A collection of all words for the hangman game."
  (->> (slurp "resources/words.txt")
       (str/split-lines )
       (map str/trim ) ))

(def words-by-length (group-by count all-words) )

(def char-freqs-by-wordlen
  "A map of character frequency maps, indexed by word length, for all-words."
    (reduce conj {}
      (for [curr-len (keys words-by-length) ]
          { curr-len 
            (frequencies (str/join (words-by-length curr-len))) } 
      )))

(comment
  (def mm {1 :a 2 :b 3 :c 4 :d 5 :e} )
  (def m2 (zipmap (keys mm) (map name (vals mm)) ))
  (def m3 (reduce conj {}
             (for [len (keys mm)] 
               { len (name (mm len)) } )))

  (doseq [len (keys char-freqs-by-wordlen)]
    (println)
    (println (format "%4s  =>  " len) (char-freqs-by-wordlen len) ) )
)

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
  [words guessed-chars]
  (let [
    ; If no chars have been guessed yet, use the pre-computed frequency map. Approximately
    ; timings for different strategies of computing char freqs are:
    ;     3100 ms:  every time;        (= -1     (count guessed-chars))
    ;     1400 ms:  all but 1st time;  (= 0      (count guessed-chars))
    ;     1230 ms:  if < 10k chars;    (< 10000  (* (count words) (count (first words))) )
    ; So, by using the precomputed char freqs on the first guess, we get a 2x gain in
    ; execution speed and sacrifice nothing in guess accuracy. Times are for all guesses
    ; on all 15 baseline words on HP p7-1254 (AMD A6-3620, Speed: 800 MHz, Cores: 4).
    char-freqs    (if (= 0      (count guessed-chars))
                    (char-freqs-by-wordlen (count (first words)) )
                    (frequencies (str/join words)) )
    avail-chars   (set/difference all-letters guessed-chars)
    best-char     (apply max-key #(get char-freqs % -1) avail-chars) 
  ] 
    best-char 
  ))

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
  (let [word-list (words-by-length (count tgt-word)) ; words of correct length
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

(defn statusString
  "Return the game status as a string."
  [hangmanGame]
  (cond 
    (HangmanUtils/isGameWon      hangmanGame ) "game-won"
    (HangmanUtils/isGameLost     hangmanGame ) "game-lost"
    (HangmanUtils/isKeepGuessing hangmanGame ) "keep-guessing" ))

(defn get-game-guessed-chars
  "Get a Clojure set of chars that have been guessed so far in the game."
  [hangmanGame]
  ; NOTE:  str/lower-case expects and returns a string. Hence we must use (apply str ...)
  ; to the java set to generate a single (possibly zero-length) string before calling
  ; str/lower-case.  Since set expects a collection, it is used without "apply".
  (let [guessed-chars  (->> (.getAllGuessedLetters hangmanGame)
                            (apply str )
                            (str/lower-case )
                            (set ) )
  ] guessed-chars ))

(defn getGameClue
  "Returns a nicely formatted clue string from the HangmanGame."
  [hangmanGame]
  (str/lower-case (.getGuessedSoFar hangmanGame)) )

(defn new-strategy
  "Return a GuessingStrategy object instance."
  []
  (reify
    GuessingStrategy
    (nextGuess [this hangmanGame]
      (let [clue            (getGameClue hangmanGame)
            guessed-chars   (get-game-guessed-chars hangmanGame)
            word-list       (words-by-length (count clue)) ; words of correct length
            keep-words      (filter-words clue guessed-chars word-list) ]
        (if (= 1 (count keep-words))
          (GuessWord. (first keep-words)) 
          (let [ new-guess (make-guess keep-words guessed-chars) ] 
            (GuessLetter. new-guess) )
        ) ))))

(defn driver
  "Driver the java interface version of the game."
  ( [] (driver "resources/test.txt") )
  ( [test-words-filename]
    (let [tst-words   (->> (slurp test-words-filename)
                           (str/split-lines)
                           (map str/trim) ) ]
      (doseq [word tst-words]
        (let [strategy      (new-strategy)
              hangmanGame   (HangmanGame. word 20) ]
          (log-dbg)
          (while (HangmanUtils/isKeepGuessing hangmanGame)
            (let [ guess     (.nextGuess strategy hangmanGame) ]
              (log-dbg "Clue:"    (format "%-20s" (getGameClue hangmanGame))
                       "  Status:"  (statusString hangmanGame) 
                       "  Guess:"   guess )
              (.makeGuess guess hangmanGame)
            ))
          (let [finalScore (.currentScore hangmanGame)]
            (log-msg "Clue:"          (format "%-20s" (getGameClue hangmanGame))
                     "  Status:"      (statusString hangmanGame)
                     "  FinalScore:"  finalScore)
            finalScore ))
        )
      )
      (log-dbg) ))

(defn main 
  [] 
  (driver)
)

(defn baseline 
  [] 
  (doseq [ tgt-word (keys baseline-scores) ]
    (let [base-score          (baseline-scores tgt-word)
          num-letter-guesses  (play-hangman tgt-word) ]
      (log-msg (str   "word:  " (format "%-15s"  tgt-word) 
                 "  guesses:  " (format   "%2s"  num-letter-guesses)
                "  baseline:  " (format   "%2s"  base-score) ))))
)

(defn -main [& args] 
  (time (apply main args)) )

