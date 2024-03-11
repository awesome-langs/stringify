#lang racket
(require srfi/13)

(struct PolyEvalType [type-str type-name value-type key-type])

(define (new-poly-eval-type__ type-str type-name value-type key-type)
    (PolyEvalType type-str type-name value-type key-type))

(define (s-to-type__ type-str)
    (if (not (regexp-match? #rx"<" type-str))
        (new-poly-eval-type__ type-str type-str #f #f)
        (letrec ([idx (string-contains type-str "<")]
              [type-name (substring type-str 0 idx)]
              [other-str (substring type-str (+ idx 1) (- (string-length type-str) 1))])
            (if (not (regexp-match? #rx"," other-str))
                (let ([value-type (s-to-type__ other-str)])
                    (new-poly-eval-type__ type-str type-name value-type #f))
                (letrec ([idx (string-contains other-str ",")]
                      [key-type (s-to-type__ (substring other-str 0 idx))]
                      [value-type (s-to-type__ (substring other-str (+ idx 1)))])
                    (new-poly-eval-type__ type-str type-name value-type key-type))))))

(define (escape-string__ s)
    (let ([new-s (map (lambda (c)
                         (cond
                             [(char=? c #\\) "\\\\"]
                             [(char=? c #\") "\\\""]
                             [(char=? c #\newline) "\\n"]
                             [(char=? c #\tab) "\\t"]
                             [(char=? c #\return) "\\r"]
                             [else (string c)]))
                      (string->list s))])
        (apply string-append new-s)))

(define (by-bool__ value)
    (if value "true" "false"))

(define (by-int__ value)
    (number->string (exact-round value)))

(define (by-double__ value)
    (let ([v (exact->inexact value)])
        (let ([vs (~r #:precision '(= 6) v)])
            (do ()
                ((not (string-suffix? "0" vs)))
                (set! vs (substring vs 0 (- (string-length vs) 1))))
            (if (string-suffix? "." vs)
                (set! vs (string-append vs "0"))
                (when (string=? vs "-0.0")
                    (set! vs "0.0")))
            vs)))

(define (by-string__ value)
    (string-append "\"" (escape-string__ value) "\""))

(define (by-list__ value ty)
    (let ([v-strs (map (lambda (v) (val-to-s__ v (PolyEvalType-value-type ty))) value)])
        (string-append "[" (string-join v-strs ", ") "]")))

(define (by-ulist__ value ty)
    (let ([v-strs (map (lambda (v) (val-to-s__ v (PolyEvalType-value-type ty))) value)])
        (string-append "[" (string-join (sort v-strs string<?) ", ") "]")))
    
(define (by-dict__ value ty)
    (let ([v-strs (hash-map value (lambda (k v) (string-append (val-to-s__ k (PolyEvalType-key-type ty)) "=>" (val-to-s__ v (PolyEvalType-value-type ty)))))])
        (string-append "{" (string-join (sort v-strs string<?) ", ") "}")))

(define (by-option__ value ty)
    (if (false? value)
        "null"
        (val-to-s__ value (PolyEvalType-value-type ty))))

(define (val-to-s__ value ty)
    (let ([type-name (PolyEvalType-type-name ty)])
        (cond
            [(string=? type-name "bool") (if (not (boolean? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-bool__ value))]
            [(string=? type-name "int") (if (not (integer? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-int__ value))]
            [(string=? type-name "double") (if (not (real? value))
                                                (error 'type-mismatch "Type mismatch")
                                                (by-double__ value))]
            [(string=? type-name "str") (if (not (string? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-string__ value))]
            [(string=? type-name "list") (if (not (list? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-list__ value ty))]
            [(string=? type-name "ulist") (if (not (list? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-ulist__ value ty))]
            [(string=? type-name "dict") (if (not (hash? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (by-dict__ value ty))]
            [(string=? type-name "option") (by-option__ value ty)]
            [else (error 'type-mismatch (string-append "Unknown type " type-name))])))

(define (stringify__ value type-str)
    (string-append (val-to-s__ value (s-to-type__ type-str)) ":" type-str))

(define tfs (string-append (stringify__ #t "bool") "\n"
            (stringify__ 3 "int") "\n"
            (stringify__ 3.141592653 "double") "\n"
            (stringify__ 3.0 "double") "\n"
            (stringify__ "Hello, World!" "str") "\n"
            (stringify__ "!@#$%^&*()\\\"\n\t" "str") "\n"
            (stringify__ '(1 2 3) "list<int>") "\n"
            (stringify__ '(#t #f #t) "list<bool>") "\n"
            (stringify__ '(3 2 1) "ulist<int>") "\n"
            (stringify__ #hash((1 . "one") (2 . "two")) "dict<int,str>") "\n"
            (stringify__ #hash(("one" . (1 2 3)) ("two" . (4 5 6))) "dict<str,list<int>>") "\n"
            (stringify__ #f "option<int>") "\n"
            (stringify__ 3 "option<int>") "\n"))
(call-with-output-file "stringify.out" (lambda (out) (display tfs out)) #:exists 'replace)