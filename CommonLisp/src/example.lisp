(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-system :alexandria)

(defun my-string-to-int (s)
    (read-from-string s))

(defun my-string-to-double (s)
    (read-from-string s))

(defun my-int-to-string (i)
    (write-to-string i))

(defun my-double-to-string (d)
    (format nil "~,6f" d))

(defun my-bool-to-string (b)
    (if b "true" "false"))

(defun my-int-to-nullable (i)
    (if (> i 0) i
        (if (< i 0) (- i)
            nil)))

(defun my-nullable-to-int (i)
    (if i i -1))

(defun my-list-sorted (lst)
    (sort lst #'string<))

(defun my-list-sorted-by-length (lst)
    (sort lst #'< :key #'length))

(defun my-list-filter (lst)
    (remove-if-not (lambda (x) (= (mod x 3) 0)) lst))

(defun my-list-map (lst)
    (mapcar (lambda (x) (* x x)) lst))

(defun my-list-reduce (lst)
    (reduce (lambda (acc x) (+ (* acc 10) x)) lst :initial-value 0))

(defun my-list-operations (lst)
    (reduce (lambda (acc x) (+ (* acc 10) x))
        (mapcar (lambda (x) (* x x))
            (remove-if-not (lambda (x) (= (mod x 3) 0)) lst))
        :initial-value 0))

(defun my-list-to-dict (lst)
    (alexandria:alist-hash-table (mapcar (lambda (x) (cons x (* x x))) lst)))

(defun my-dict-to-list (dict)
    (mapcar (lambda (x) (+ (car x) (cdr x))) 
        (sort (alexandria:hash-table-alist dict) #'< :key #'car)))

(defun my-print-string (s)
    (format t "~a~%" s))

(defun my-print-string-list (lst)
    (dolist (x lst)
        (format t "~a" (concatenate 'string x " ")))
    (format t "~%"))

(defun my-print-int-list (lst)
    (my-print-string-list (mapcar #'my-int-to-string lst)))

(defun my-print-dict (dict)
    (maphash (lambda (k v)
        (format t "~a" (concatenate 'string (my-int-to-string k) "->" (my-int-to-string v) " "))) dict)
    (format t "~%"))

(my-print-string "Hello, World!")
(my-print-string (my-int-to-string (my-string-to-int "123")))
(my-print-string (my-double-to-string (my-string-to-double "123.456")))
(my-print-string (my-bool-to-string nil))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 18))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable -15))))
(my-print-string (my-int-to-string (my-nullable-to-int (my-int-to-nullable 0))))
(my-print-string-list (my-list-sorted '("e" "dddd" "ccccc" "bb" "aaa")))
(my-print-string-list (my-list-sorted-by-length '("e" "dddd" "ccccc" "bb" "aaa")))
(my-print-string (my-int-to-string (my-list-reduce (my-list-map (my-list-filter '(3 12 5 8 9 15 7 17 21 11))))))
(my-print-string (my-int-to-string (my-list-operations '(3 12 5 8 9 15 7 17 21 11))))
(my-print-dict (my-list-to-dict '(3 1 4 2 5 9 8 6 7 0)))
(my-print-int-list (my-dict-to-list (alexandria:alist-hash-table '((3 . 9) (1 . 1) (4 . 16) (2 . 4) (5 . 25) (9 . 81) (8 . 64) (6 . 36) (7 . 49) (0 . 0)))))