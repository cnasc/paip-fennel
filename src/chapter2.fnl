;; Chapter 2 -- A Simple Lisp Program

;; I will just make a translation of the program here and then
;; annotate it in a post highlighting some of the differences.

;; Sentence => Noun-Phrase + Verb-Phrase
;; Noun-Phrase => Article + Noun
;; Verb-Phrase => Verb + Noun-Phrase
;; Article => the, a ...
;; Noun => man, ball, woman, table ...
;; Verb => hit, took, saw, liked ...

(set inspect (require :inspect))

;; Just going to paste this until I know how to handle local imports
(set append
     (fn [...] ; [1]
       "Return a new table containing the concatenation of all tables in `...`"
       (let [result []
             tbls [ ... ]] ; [2]
         (each [i tbl (ipairs tbls)]
           (each [i val (ipairs tbl)]
             (table.insert result val)))
         result)))

(set map
     (fn [f tbl]
       "Creates a new table from the results of applying `f` to each element of tbl"
       (let [result []]
         (each [i v (ipairs tbl)]
           (table.insert result (f v)))
         result)))

(set mappend
     (fn [f tbl]
       "Apply f to each element of the list and append the results"
       (append (table.unpack (map f tbl)))))

(set table? (fn [x] (= (type x) :table)))

(set sentence (fn [] (append (noun-phrase) (verb-phrase))))
(set noun-phrase (fn [] (append (article) (noun))))
(set verb-phrase (fn [] (append (verb) (noun-phrase))))
(set article (fn [] (one-of ["the" "a"])))
(set noun (fn [] (one-of ["man" "ball" "woman" "table"])))
(set verb (fn [] (one-of ["hit" "took" "saw" "liked"])))

(set random-elt
     (fn [tbl]
       (. tbl (math.random (# tbl)))))

(set one-of
     (fn [tbl]
       "Assuming that tbl is array-like (only integer keys)"
       [ (random-elt tbl) ]))

(inspect (sentence)) ;; => { "the", "man", "hit", "a", "man" } (will vary)

;; 2.3 -- A Rule-Based Solution

;; Make it easier to write grammar rules. This approach is more
;; data-driven and requires less code when things change.

;; Are earmuffs good Fennel style? I think it's helpful to call out
;; global variables.
(set *simple-grammar* {:sentence [[:noun-phrase :verb-phrase]]
                       :noun-phrase [[:article :noun]]
                       :verb-phrase [[:verb :noun-phrase]]
                       :article ["the" "a"]
                       :noun ["man" "ball" "woman" "table"]
                       :verb ["hit" "took" "saw" "liked"]})

(set *grammar* *simple-grammar*)

;; we're using a real associative array here, so no need for `assoc`
;; `.` will do quite nicely.
;; Also we can do this in just one function now, rather than 3
(set rewrites (fn [category] (. *grammar* category)))

(set generate
     (fn [phrase]
       "Generate a random sentence or phrase."
       (if (table? phrase) (mappend generate phrase)
           (rewrites phrase) (generate (random-elt (rewrites phrase)))
           :else [ phrase ])))

(inspect (generate :sentence)) ;; => { "the", "table", "hit", "a", "ball" }
(inspect (generate :noun-phrase)) ;; => { "the", "table" }
(inspect (generate :verb-phrase)) ;; => { "took", "the", "ball" }
(inspect (generate :article)) ;; => { "a" }
(inspect (generate :noun)) ;; => { "ball" }
(inspect (generate :verb)) ;; => { "hit" }

;; Exercise 2.1 -- Write a version of generate that uses cond but
;; avoids calling `rewrites` twice
(set generate21
     (fn [phrase]
       "Generate a random sentence or phrase."
       (let [rws (rewrites phrase)]
         (if (table? phrase) (mappend generate phrase)
             rws (generate (random-elt rws))
             :else [ phrase ]))))

;; All non-space chars are now allowed in :keywords on the Fennel
;; master branch -- I will leave the -star suffix here until that
;; change lands in a Luarocks release.
(set *bigger-grammar* {:sentence [[:noun-phrase :verb-phrase]]
                       :noun-phrase [[:article :adj-star :noun :pp-star] [:name] [:pronoun]]
                       :verb-phrase [[:verb :noun-phrase :pp-star]]
                       :pp-star [[] [:pp :pp-star]]
                       :adj-star [[] [:adj :adj-star]]
                       :pp [[:prep :noun-phrase]]
                       :prep ["to" "in" "by" "with" "on"]
                       :adj ["big" "little" "blue" "green" "adiabatic"]
                       :article ["the" "a"]
                       :name ["Pat" "Kim" "Lee" "Terry" "Robin"]
                       :noun ["man" "ball" "woman" "table"]
                       :verb ["hit" "took" "saw" "liked"]
                       :pronoun ["he" "she" "it" "these" "those" "that"]})

(set *grammar* *bigger-grammar*)

;; The more complicated grammar can make more sophisticated sentences,
;; but also is more likely to generate utter insanity.
(inspect (generate :sentence)) ;; => { "Pat", "took", "Kim", "in",
                               ;; "the", "table", "on", "he", "in",
                               ;; "Kim", "on", "that", "in", "a",
                               ;; "woman", "by", "the", "little",
                               ;; "table", "with", "these", "to",
                               ;; "the", "table", "by", "these" }


