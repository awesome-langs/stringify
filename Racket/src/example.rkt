#lang racket
(require srfi/13)

(struct PolyEvalType [type-str type-name value-type key-type])

(define (__new-poly-eval-type type-str type-name value-type key-type)
    (PolyEvalType type-str type-name value-type key-type))

(define (__s-to-type type-str)
    (if (not (regexp-match? #rx"<" type-str))
        (__new-poly-eval-type type-str type-str #f #f)
        (letrec ([idx (string-contains type-str "<")]
              [type-name (substring type-str 0 idx)]
              [other-str (substring type-str (+ idx 1) (- (string-length type-str) 1))])
            (if (not (regexp-match? #rx"," other-str))
                (let ([value-type (__s-to-type other-str)])
                    (__new-poly-eval-type type-str type-name value-type #f))
                (letrec ([idx (string-contains other-str ",")]
                      [key-type (__s-to-type (substring other-str 0 idx))]
                      [value-type (__s-to-type (substring other-str (+ idx 1)))])
                    (__new-poly-eval-type type-str type-name value-type key-type))))))

(define (__escape-string s)
    (let ([new-s ""])
        (for ([c s])
            (cond
                [(char=? c #\\) (set! new-s (string-append new-s "\\\\"))]
                [(char=? c #\") (set! new-s (string-append new-s "\\\""))]
                [(char=? c #\newline) (set! new-s (string-append new-s "\\n"))]
                [(char=? c #\tab) (set! new-s (string-append new-s "\\t"))]
                [(char=? c #\return) (set! new-s (string-append new-s "\\r"))]
                [else (set! new-s (string-append new-s (string c)))])
        )
        new-s))

(define (__by-bool value)
    (if value "true" "false"))

(define (__by-int value)
    (number->string (exact-round value)))

(define (__by-double value)
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

(define (__by-string value)
    (string-append "\"" (__escape-string value) "\""))

(define (__by-list value ty)
    (let ([v-strs (list)])
        (for ([v value])
            (set! v-strs (append v-strs (list (__val-to-s v (PolyEvalType-value-type ty))))))
        (let ([ret "["])
            (for ([i (in-range (length v-strs))])
                (set! ret (string-append ret (list-ref v-strs i)))
                (when (< i (- (length v-strs) 1))
                    (set! ret (string-append ret ", "))))
            (string-append ret "]"))))

(define (__by-ulist value ty)
    (let ([v-strs (list)])
        (for ([v value])
            (set! v-strs (append v-strs (list (__val-to-s v (PolyEvalType-value-type ty))))))
        (set! v-strs (sort v-strs string<?))
        (let ([ret "["])
            (for ([i (in-range (length v-strs))])
                (set! ret (string-append ret (list-ref v-strs i)))
                (when (< i (- (length v-strs) 1))
                    (set! ret (string-append ret ", "))))
            (string-append ret "]"))))

(define (__by-dict value ty)
    (let ([v-strs (list)])
        (for ([(key val) value])
            (set! v-strs (append v-strs (list (string-append (__val-to-s key (PolyEvalType-key-type ty)) "=>" (__val-to-s val (PolyEvalType-value-type ty)))))))
        (set! v-strs (sort v-strs string<?))
        (let ([ret "{"])
            (for ([i (in-range (length v-strs))])
                (set! ret (string-append ret (list-ref v-strs i)))
                (when (< i (- (length v-strs) 1))
                    (set! ret (string-append ret ", "))))
            (string-append ret "}"))))

(define (__by-option value ty)
    (if (false? value)
        "null"
        (__val-to-s value (PolyEvalType-value-type ty))))

(define (__val-to-s value ty)
    (let ([type-name (PolyEvalType-type-name ty)])
        (cond
            [(string=? type-name "bool") (if (not (boolean? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-bool value))]
            [(string=? type-name "int") (if (not (integer? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-int value))]
            [(string=? type-name "double") (if (not (real? value))
                                                (error 'type-mismatch "Type mismatch")
                                                (__by-double value))]
            [(string=? type-name "str") (if (not (string? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-string value))]
            [(string=? type-name "list") (if (not (list? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-list value ty))]
            [(string=? type-name "ulist") (if (not (list? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-ulist value ty))]
            [(string=? type-name "dict") (if (not (hash? value))
                                            (error 'type-mismatch "Type mismatch")
                                            (__by-dict value ty))]
            [(string=? type-name "option") (__by-option value ty)]
            [else (error 'type-mismatch (string-append "Unknown type " type-name))])))

(define (__stringify value type-str)
    (string-append (__val-to-s value (__s-to-type type-str)) ":" type-str))

(define tfs (string-append (__stringify #t "bool") "\n"
            (__stringify 3 "int") "\n"
            (__stringify 3.141592653 "double") "\n"
            (__stringify 3.0 "double") "\n"
            (__stringify "Hello, World!" "str") "\n"
            (__stringify "!@#$%^&*()\\\"\n\t" "str") "\n"
            (__stringify '(1 2 3) "list<int>") "\n"
            (__stringify '(#t #f #t) "list<bool>") "\n"
            (__stringify '(3 2 1) "ulist<int>") "\n"
            (__stringify #hash((1 . "one") (2 . "two")) "dict<int,str>") "\n"
            (__stringify #hash(("one" . (1 2 3)) ("two" . (4 5 6))) "dict<str,list<int>>") "\n"
            (__stringify #f "option<int>") "\n"
            (__stringify 3 "option<int>") "\n"))
(call-with-output-file "stringify.out" (lambda (out) (display tfs out)) #:exists 'replace)