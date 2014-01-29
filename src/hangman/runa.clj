(ns code-puzzle.core
  (:require
    [compojure.core      :as compojure]
    [compojure.handler   :as handler]
    [compojure.route     :as route] 
    [clojure.data.json   :as json] 
    [hiccup.core         :as hc]
    [hiccup.page         :as hp]
    [garden.core         :as gd]
  ) )

(def run-validate  true )  ; -> set true to see validation printouts
(def debug-print   false)  ; -> set true to see debug printouts

; The input file names for merchant and runa data
(def file-names 
  { :runa      "resources/runa_data.csv" 
    :merchant  "resources/merchant_data.psv"  } )

; The type and order of runa data fields as read in
(def hdr-line-runa 
  "Order ID,Unit Price Cents,Merchant Discount Cents,Runa Discount Cents,Session Type" )

; Runa sample data
(def test-data-runa [
  "72144305,11050,1000,2020,control"
  "72144777,20000,0,3000,test"
  "72145239,5000,500,1500,unmanaged" ] )

; The type and order of merchant data fields as read in
(def hdr-line-merchant 
  "Order ID|Unit Price Dollars|Runa Discount Dollars|Merchant Discount Dollars|Session Type" )

; Merchant sample data
(def test-data-merchant [
  "72144305|110.0|20.5|10.2|CONTROL"
  "72144777|200.0|30.0|0.0|TEST"
  "72145239|50.0|14.8|2.0|UNMANAGED" ] )

;********************************************************************************
; ASSUMPTIONS:  The sample data imply several things:
;   1. Both data files will have exactly 1 match for a given order-id.
;   2. Fields order-id and session-type are treated as strings, coerced to lowercase; all
;      others are treated as BigDecimal values.
;   3. For each order-id, we may sort on either the runa or merchant values
;      of session-type or unit-price-dollars.  We arbitrarily choose to sort on the runa
;      value for these 2 fields.
;********************************************************************************

; Note:  cannot leave trailing zeros in BigDecimal values values converted from cents or
; equality test will fail 
(def expected-result-runa [ 
    { :order-id "72144305" :unit-price-dollars 110.5M :merchant-discount-dollars 10.M 
    :runa-discount-dollars 20.2M :session-type "control" }

    { :order-id "72144777" :unit-price-dollars 200.M  :merchant-discount-dollars 0.M 
    :runa-discount-dollars 30.M  :session-type "test" }

    { :order-id "72145239"  :unit-price-dollars 50.M  :merchant-discount-dollars 5.M 
    :runa-discount-dollars 15.M  :session-type "unmanaged" }
  ] )

(def expected-result-merchant [ 
    { :order-id   "72144305"   :unit-price-dollars   110.0M   :runa-discount-dollars   20.5M   
    :merchant-discount-dollars   10.2M   :session-type "control" }

    { :order-id   "72144777"   :unit-price-dollars   200.0M   :runa-discount-dollars   30.0M   
    :merchant-discount-dollars    0.0M  :session-type "test" }

    { :order-id   "72145239"   :unit-price-dollars    50.0M   :runa-discount-dollars   14.8M   
    :merchant-discount-dollars    2.0M  :session-type "unmanaged" } 
  ] )

(def expected-json-str
  "{ \"summaries\":{\"runa-summary\":{ \"unit-price-dollars\":360.5,
                                       \"merchant-discount-dollars\":15.0,
                                       \"runa-discount-dollars\":65.2}, 
                     \"merchant-summary\":{ \"unit-price-dollars\":360.0, 
                                            \"merchant-discount-dollars\":12.2,
                                            \"runa-discount-dollars\":65.3}},

     \"orders\":[{\"runa-data\":{\"unit-price-dollars\":50.0,
                                               \"merchant-discount-dollars\":5.0, 
                                               \"runa-discount-dollars\":15.0,
                                               \"session-type\":\"unmanaged\",
                                               \"order-id\": \"72145239\"}, 
                                 \"merchant-data\":{\"unit-price-dollars\":50.0,
                                                    \"merchant-discount-dollars\":2.0, 
                                                    \"runa-discount-dollars\":14.8,
                                                    \"session-type\":\"unmanaged\",
                                                    \"order-id\": \"72145239\"}}, 

                 {\"runa-data\":{\"unit-price-dollars\":200.0,
                                               \"merchant-discount-dollars\":0.0,
                                               \"runa-discount-dollars\":30.0,
                                               \"session-type\":\"test\",
                                               \"order-id\": \"72144777\"},
                                \"merchant-data\":{\"unit-price-dollars\":200.0,
                                                   \"merchant-discount-dollars\":0.0,
                                                   \"runa-discount-dollars\":30.0,
                                                   \"session-type\":\"test\",
                                                   \"order-id\": \"72144777\"}},

                  {\"runa-data\":{\"unit-price-dollars\":110.5,
                                               \"merchant-discount-dollars\":10.0,
                                               \"runa-discount-dollars\":20.2,
                                               \"session-type\":\"control\",
                                               \"order-id\": \"72144305\"},
                                \"merchant-data\":{\"unit-price-dollars\":110.0,
                                                   \"merchant-discount-dollars\":10.2,
                                                   \"runa-discount-dollars\":20.5,
                                                   \"session-type\":\"control\",
                                                   \"order-id\": \"72144305\"}}]}}"
)


(defn csv->vec 
  "Convert a CSV (comma-separated-value) string into a vector of text strings."
  [^String line]
  (vec (re-seq #"[^,]+" line )) )

(defn psv->vec 
  "Convert a PSV (pipe-separated-value) string into a vector of text strings."
  [^String line]
  (vec (re-seq #"[^|]+" line )) )

(defn parse-decimal "Parse a string into a BigDecimal value"
  [^String s]
  (java.math.BigDecimal. s) )

(defn parse-int "Parse a string into an Integer value"
  [^String s]
  (java.lang.Integer. s) )

(defn cent->dollar  "Convert cents to BigDecimal dollars"
  [^Number cents]
  (bigdec (/ cents 100)) )


(defn parse-line-runa
  "Parse a single line from the runa data file, returning a standard map of data values."
  [^String line]
  (let [ ; Destructuring order must match hdr-line-runa
         [ order-id unit-price-cents merchant-discount-cents runa-discount-cents 
           session-type :as all-fields ]  (csv->vec line) 
         data-map     { :order-id                    (.toLowerCase            order-id )
                        :unit-price-dollars          (cent->dollar (parse-int unit-price-cents ))
                        :merchant-discount-dollars   (cent->dollar (parse-int merchant-discount-cents ))
                        :runa-discount-dollars       (cent->dollar (parse-int runa-discount-cents ))
                        :session-type                (.toLowerCase            session-type  )
                      } 
       ]
    ; Basic input validation.  Add more as needed.
    (when-not (= 5 (count all-fields) )
      (throw (java.lang.IllegalArgumentException. 
        (str "Invalid number of input fields: " all-fields ) ) ) )
    data-map ) )

(defn parse-line-merchant
  "Parse a single line from the merchant data file, returning a standard map of data values."
  [^String line]
  (let [ ; Destructuring order must match hdr-line-merchant
         [ order-id unit-price-dollars runa-discount-dollars merchant-discount-dollars 
           session-type :as all-fields ]  (psv->vec line)
         data-map     { :order-id                    (.toLowerCase  order-id )
                        :unit-price-dollars          (parse-decimal unit-price-dollars )
                        :runa-discount-dollars       (parse-decimal runa-discount-dollars )
                        :merchant-discount-dollars   (parse-decimal merchant-discount-dollars )
                        :session-type                (.toLowerCase  session-type )
                      } 
       ]
    ; Basic input validation.  Add more as needed.
    (when-not (= 5 (count all-fields) )
      (throw (java.lang.IllegalArgumentException. 
        (str "Invalid number of input fields: " all-fields ) ) ) )
    data-map ) )

(defn parse-lines-runa 
  "Parse data lines from runa file, returning a sequence of standard maps"
  [data-lines]
   (map parse-line-runa data-lines ) )

(defn parse-lines-merchant 
  "Parse data lines from merchant file, returning a sequence of standard maps"
  [data-lines]
   (map parse-line-merchant data-lines ) )

(defn do-tests [] 
  (assert (= (csv->vec "72144305,11050,1000,2020,control" )
                      ["72144305" "11050" "1000" "2020" "control"] ))
  (assert (= (psv->vec "72144305|110.0|20.5|10.2|CONTROL")
                      ["72144305" "110.0" "20.5" "10.2" "CONTROL"] ))

  ; Note that missing values result in a shorter output vector, and don't result in
  ; zero-length strings in the output vector.
  (assert (= (csv->vec "a,,,bb,ccc" ) ["a" "bb" "ccc"] ))
  (assert (= (psv->vec "a|||bb|ccc" ) ["a" "bb" "ccc"] ))

  ; Normal path
  (assert (= (parse-int     "123"   ) 123    ))
  (assert (= (parse-decimal "123.4" ) 123.4M ))
  (assert (= (cent->dollar   12340  ) 123.4M ))

  ; Watch out for trailing zeros with bigdec values
  (assert (not (=                 123.4M    123.40M )))
  (assert (not (= (parse-decimal "123.40" ) 123.4M  )))
  (assert (not (= (cent->dollar   12340   ) 123.40M )))

  (println)
  (println "TESTING:")
  (println "  passed basic" )

  (let [ result (parse-lines-runa test-data-runa) ]
    (when debug-print
      (println)
      (println "********************************************************************************")
      (println "do-tests: runa (actual):")
      (println)
      (doseq [ result-line result ]
        (println result-line) )
      (println)
      (println "do-tests: runa (expected):")
      (println)
      (doseq [ result-line expected-result-runa ]
        (println result-line) ) 
      (println) )
    (assert (= result expected-result-runa ))
    (println "  passed expected-result-runa " ) )


  (let [ result (parse-lines-merchant test-data-merchant ) ]
    (when debug-print
      (println)
      (println "********************************************************************************")
      (println "do-tests: merchant:")
      (println)
      (doseq [ result-line result ]
        (println result-line) )
      (println)
      (println "do-tests: merchant (expected):")
      (println)
      (doseq [ result-line expected-result-merchant ]
        (println result-line) ) 
      (println) )
    (assert (= result expected-result-merchant ))
    (println "  passed expected-result-merchant " ) ) )

(defn parse-file-runa []
  (with-open [ rdr (clojure.java.io/reader (:runa file-names) ) ] 
    (let [ all-lines     (doall (line-seq rdr))
           header-line   (first all-lines)
           data-lines    (rest  all-lines)
           result-seq    (parse-lines-runa data-lines) ]
      ; Basic input validation.  Add more as needed.
      (when-not (= header-line hdr-line-runa)
        (throw (java.lang.IllegalArgumentException. 
          (str "parse-file-runa: invalid header-line='" header-line "'" ) ) ) )
      result-seq ) ) )

(defn parse-file-merchant []
  (with-open [ rdr (clojure.java.io/reader (:merchant file-names) ) ] 
    (let [ all-lines     (doall (line-seq rdr))
           header-line   (first all-lines)
           data-lines    (rest  all-lines)
           result-seq    (parse-lines-merchant data-lines) ]
      ; Basic input validation.  Add more as needed.
      (when-not (= header-line hdr-line-merchant)
        (throw (java.lang.IllegalArgumentException. 
          (str "parse-file-merchant invalid header-line='" header-line "'" ) ) ) )
      result-seq ) ) )

(def summary-field-keys 
  [ :unit-price-dollars :runa-discount-dollars :merchant-discount-dollars ] )

(defn create-summary 
  "Returns a summary map of the summary-field-keys for a sequence of maps."
  [data-seq]
  (into {}
    (for [ field-key summary-field-keys ]
      [ field-key (apply + (map field-key data-seq)) ] )) )

; Each key in this map represents a desired sort order, and each value is the function
; which implements that sort order.
(def sort-order-fns {
  :session-type-desc 
      (fn [data-seq] 
         (sort-by #(get-in % [:runa-data :session-type])  (comp - compare)  data-seq ) )
  :order-id-asc 
      (fn [data-seq]
          (sort-by #(get-in % [:runa-data :order-id])  data-seq ) )
  :unit-price-dollars-asc 
      (fn [data-seq]
          (sort-by #(get-in % [:runa-data :unit-price-dollars])  data-seq ) )
  } )

(defn valid-sort-order? 
  "Predicate indicating if a sort-order is valid"
  [order-by]
  (sort-order-fns (keyword order-by)) )

(defn generate-report 
  "Generate report map comparing runa & merchant data (with summary) in the specified sort
  order"
  [order-by] 

  (let [ data-runa     (parse-file-runa)
         data-merchant (parse-file-merchant)
         sort-order-fn (sort-order-fns order-by)

         report-map { 
             :summaries 
               { :runa-summary     (create-summary data-runa) 
                 :merchant-summary (create-summary data-merchant) }

             :orders
               (->>
                 ; Note: Nested for is order N^2 operation, but should be OK for
                 ; anything being reviewed by a human.
                 (for [ order-runa      data-runa
                        order-merchant  data-merchant 
                        :when           (= (:order-id order-runa) 
                                           (:order-id order-merchant) ) ]
                    { :runa-data     order-runa
                      :merchant-data order-merchant } )
                 sort-order-fn 
                 (vec) )
           } ]
    report-map
  )
)

(defn json-read-num 
  "Helper function for clojure.data.json/read-str. For map [key value] pairs, when value
  is numeric transforms to java.lang.Double since this is the only numeric type in
  JavaScript. "
    ; Since clojure.data.json/write-str doesn't like to print a decimal fraction for
    ; integer values (even if their type is BigDecimal) the JSON string notation for maps
    ; will always look like {"unit-price-dollars":15} instead of
    ; {"unit-price-dollars":15.0}. However, the puzzle expected results has exactly 1
    ; decimal digit specified for all dollar amounts, which destroys the equality test in
    ; Clojure where 15 and 15.0 are NOT equal.  Since the puzzle states that the web
    ; service result must equal the expected result under JSON equality, we convert all
    ; numbers to JSON/JavaScript double-precision before testing for equality. Then,
    ; everything works!  :)
  [ key-arg val-arg ]
  (if (isa? (class val-arg) java.lang.Number)
    (double val-arg)
    val-arg ) )

(defn json-write-num 
  "Dual of json-read-num, used for output."
    ; Used for testing only at present
  [ key-arg val-arg ]
  (if (isa? (class val-arg) java.lang.Number)
    (format "%.1f" (bigdec val-arg))
    val-arg ) )

(defn validation-test []
  (when run-validate
    (println)
    (println "********************************************************************************")
    (println "VALIDATION TEST DATA:")

    (println)
    (println "format test:")
    (let [ xx  { :number 2M :price 15.0M } ]
      (println "  plain:  " (json/write-str xx) )
      (println "  fixed:  " (json/write-str xx :value-fn json-write-num) ) )

    (let [
      result-json-str     (json/write-str (generate-report    :session-type-desc) ) 
      result-json-data    (json/read-str   result-json-str    :value-fn json-read-num)
      expected-json-data  (json/read-str   expected-json-str  :value-fn json-read-num) ] 

      (println)
      (println "expected-json-str")
      (println  expected-json-str )

      (println)
      (println "result-json-str")
      (println  result-json-str )

      (println)
      (println "expected-json-data")
      (println (with-out-str (json/pprint expected-json-data )) )

      (println)
      (println "result-json-data")
      (println (with-out-str (json/pprint result-json-data )) )

      (let [ equality-test-result  (= expected-json-data result-json-data) ]
        (println)
        (println "equality test: " equality-test-result)
        (assert equality-test-result "equality-test-result") )
    ) 

    (println)
    (println "END VALIDATION TEST")
    (println "********************************************************************************")
  )
)

(defn -main 
  "Run tests and validation"
  []
  (do-tests) 
  (validation-test) )

(defn main [] (-main))

(defn default-page  "Returns html for the default page '/' "
  []
  (let [
    styles [
        [ "*"  {:margin "0px" :padding "0px" }  ]
        [ :h1    { :font "bold 30px verdana, sans-serif" } ]
        [ :h2    { :font "bold 24px verdana, sans-serif" } ]
        [ :h3    { :font "bold 18px verdana, sans-serif" } ]
        [ :time  { :font "12px verdana, sans-serif" } ]
        [ :header :section :footer :aside :nav :article :figure :figcaption :hgroup
          { :display :block } ]
        [ :body { :text-align :left } ]
        [ "#wrapper" { :width "800px"  :margin "15px auto"  :text-align :left } ]
        [ "#main_header" { :background "#ccccff"  :border "1px solid #999999" :padding "20px"
                           :text-align :center  } ]
        [ "#main_figure" { :padding "15px" :text-align :center  
                           :font "8px verdana, sans-serif" } ]
        [ "#main_section" { :padding "5px 15px" } ]
        [ "#main_section" 
          [ :li { :list-style :none  :padding "5px 25px" 
                  :font "bold 14px verdana, sans-serif" } ] ]
        [ "#main_footer" { :padding "25px" :text-align :center 
                           :font "bold 14px verdana, sans-serif" } ] 
      ]
    head 
      [ :head 
        [ :meta { :charset "utf-8" } ]
        [ :meta { :name "description" :content "This is an example" } ]
        [ :meta { :name "keywords" :content "HTML5, CSS3, JavaScript" } ]
        [ :title "Code Puzzle "]
        [ :style  (gd/css  {:output-style :expanded}  styles) ] ] 
    body 
      [ :div { :id "wrapper" }
        [ :body 
          [ :header { :id "main_header" }
            [ :h1 "Welcome to Runatic!" ] ]
          [ :figure { :id "main_figure" }
            [ :img { :src "runa2.png" } ] 
            [ :i [ :figcaption "I used to be Runa" ] ] ]
          [ :section { :id "main_section" }
            [ :h2 "Valid URIs are:"
              [ :ul
                [:li [:a {:href "/runatic/report?order_by=session-type-desc" } 
                                "/runatic/report?order_by=session-type-desc" ] ]
                [:li [:a {:href "/runatic/report?order_by=order-id-asc" } 
                                "/runatic/report?order_by=order-id-asc" ] ]
                [:li [:a {:href "/runatic/report?order_by=unit-price-dollars-asc" } 
                                "/runatic/report?order_by=unit-price-dollars-asc" ] ] ]
              [ :time { :datetime "2012-12-10" :pubdate :nil }
                "posted Dec 16, 2013" ] ] ]
          [ :footer { :id "main_footer" }
            [ :small "Copyright &copy; 2013" ] ] ] ]
    ] ; let
    (hp/html5  head body ) 
  ) ; let 
)

(compojure/defroutes app-routes
  (compojure/GET "/" [] 
    (default-page) )

  (compojure/GET "/runatic/report" [order_by]
    (if (valid-sort-order? order_by)
      (str  "<pre>" 
            (with-out-str (json/pprint (generate-report (keyword order_by) )))
            "</pre>" )
      (str "<h2> *** Runatic Report *** </h2>  "
           "<h3> INVALID SORT ORDER: order_by='" order_by "' </h3> " ) ) )

  (route/resources "/")
  (route/not-found "Not Found") )

(def app
  (handler/site app-routes) )
