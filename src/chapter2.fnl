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

(set sentence (fn [] (append (noun-phrase) (verb-phrase))))
(set noun-phrase (fn [] (append (article) (noun))))
(set verb-phrase (fn [] (append (verb) (noun-phrase))))
(set article (fn [] (one-of ["the" "a"])))
(set noun (fn [] (one-of ["man" "ball" "woman" "table"])))
(set verb (fn [] (one-of ["hit" "took" "saw" "liked"])))

(set one-of
     (fn [tbl]
       "Assuming that tbl is array-like (only integer keys)"
       [ (. tbl (math.random (# tbl))) ]))

(inspect (sentence)) ;; => { "the", "man", "hit", "a", "man" } (will vary)

;; 2.3 -- A Rule-Based Solution

;; Make it easier to write grammar rules. This approach is more
;; data-driven and requires less code when things change.

;; Are earmuffs good Fennel style? I think it's helpful to call out
;; global variables.
(set *simple-grammar* {:sentence [:noun-phrase :verb-phrase]
                       :noun-phrase [:article :noun]
                       :verb-phrase [:verb :noun-phrase]
                       :article ["the" "a"]
                       :noun ["man" "ball" "woman" "table"]
                       :verb ["hit" "took" "saw" "liked"]})

(set *grammar* *simple-grammar*)

;; we're using a real associative array here, so no need for `assoc`
;; `.` will do quite nicely.
;; Also we can do this in just one function now, rather than 3
(set rewrites (fn [category] (. *grammar* category)))


