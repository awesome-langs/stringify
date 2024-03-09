#lang racket
(require racket/dict)

(define (my-string-to-int s)
    (string->number s))

(define (my-string-to-double s) 
    (string->number s))

(define (my-int-to-string i)
    (number->string i))

(define (my-double-to-string d)
    (~r #:precision '(= 6) d))

(define (my-bool-to-string b)
    (if b "true" "false"))

(define (my-int-to-nullable i)
    (cond
        [(> i 0) i]
        [(< i 0) (- i)]
        [else #f]))

(define (my-nullable-to-int i)
    (if i i -1))

(define (my-list-sorted lst)
    (sort lst string<?))

(define (my-list-sorted-by-length lst)
    (sort lst (λ (a b) (< (string-length a) (string-length b)))))

(define (my-list-filter lst)
    (filter (λ (x) (= (remainder x 3) 0)) lst))

(define (my-list-map lst)
    (map (λ (x) (* x x)) lst))

(define (my-list-reduce lst)
    (foldl (λ (x acc) (+ (* acc 10) x)) 0 lst))

(define (my-list-operations lst)
    (foldl (λ (x acc) (+ (* acc 10) x)) 0 
        (map (λ (x) (* x x)) 
            (filter (λ (x) (= (remainder x 3) 0)) lst))))

(define (my-list-to-dict lst)
    (make-hash (map (λ (x) (cons x (* x x))) lst)))

(define (my-dict-to-list dict)
    (map (λ (x) (+ (car x) (cdr x)))
        (sort (dict->list dict)
            (λ (a b) (< (car a) (car b))))))

(define (my-print-string s)
    (displayln s))

(define (my-print-string-list lst)
    (for ([x lst]) 
        (display (string-append x " ")))
    (displayln ""))

(define (my-print-int-list lst)
    (my-print-string-list (map my-int-to-string lst)))

(define (my-print-dict dict)    
    (for ([(k v) dict]) 
        (display (string-append (my-int-to-string k) "->" (my-int-to-string v) " ")))
    (displayln ""))

(my-print-string "Hello, World!")
(my-print-string (my-int-to-string (my-string-to-int "123")))
(my-print-string (my-double-to-string (my-string-to-double "123.456")))
(my-print-string (my-bool-to-string #f))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 18))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable -15))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 0))))
(my-print-string-list (my-list-sorted '("e" "dddd" "ccccc" "bb" "aaa")))
(my-print-string-list (my-list-sorted-by-length '("e" "dddd" "ccccc" "bb" "aaa")))
(my-print-string (my-int-to-string (my-list-reduce (my-list-map (my-list-filter '(3 12 5 8 9 15 7 17 21 11))))))
(my-print-string (my-int-to-string (my-list-operations '(3 12 5 8 9 15 7 17 21 11))))
(my-print-dict (my-list-to-dict '(3 1 4 2 5 9 8 6 7 0)))
(my-print-int-list (my-dict-to-list #hash((3 . 9) (1 . 1) (4 . 16) (2 . 4) (5 . 25) (9 . 81) (8 . 64) (6 . 36) (7 . 49) (0 . 0))))
