(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-system :alexandria)
(defstruct poly-eval-type type-str type-name value-type key-type)

(defun new-poly-eval-type__ (type-str type-name value-type key-type)
    (make-poly-eval-type :type-str type-str :type-name type-name :value-type value-type :key-type key-type))

(defun s-to-type__ (type-str)
    (if (not (search "<" type-str))
        (new-poly-eval-type__ type-str type-str nil nil)
        (let* ((idx (position #\< type-str))
              (type-name (subseq type-str 0 idx))
              (other-str (subseq type-str (+ 1 idx) (- (length type-str) 1))))
            (if (not (search "," other-str))
                (let ((value-type (s-to-type__ other-str)))
                    (new-poly-eval-type__ type-str type-name value-type nil))
                (let* ((idx (position #\, other-str))
                      (key-type (s-to-type__ (subseq other-str 0 idx)))
                      (value-type (s-to-type__ (subseq other-str (+ 1 idx)))))
                    (new-poly-eval-type__ type-str type-name value-type key-type))))))

(defun escape-string__ (s)
    (let ((new-s (mapcar (lambda (c)
        (cond ((char= c #\\) "\\\\")
              ((char= c #\") "\\\"")
              ((char= c #\newline) "\\n")
              ((char= c #\tab) "\\t")
              ((char= c #\return) "\\r")
              (t (string c)))) (coerce s 'list))))
        (apply #'concatenate 'string new-s)))

(defun by-bool__ (value)
    (if value "true" "false"))


(defun by-int__ (value)
    (write-to-string (truncate value)))


(defun by-double__ (value)
    (let ((vs (format nil "~,6f" value)))
        (loop while (string= (subseq vs (- (length vs) 1)) "0") do
            (setf vs (subseq vs 0 (- (length vs) 1))))
        (if (string= (subseq vs (- (length vs) 1)) ".")
            (setf vs (concatenate 'string vs "0"))
            (if (string= vs "-0.0")
                (setf vs "0.0")))
        vs))

(defun by-string__ (value)
    (concatenate 'string "\"" (escape-string__ value) "\""))

(defun by-list__ (value ty)
    (let ((v-strs (mapcar (lambda (v) (val-to-s__ v (slot-value ty 'value-type))) value)))
        (format nil "[~{~A~^, ~}]" v-strs)))

(defun by-ulist__ (value ty)
    (let ((v-strs (mapcar (lambda (v) (val-to-s__ v (slot-value ty 'value-type))) value)))
        (format nil "[~{~A~^, ~}]" (sort v-strs #'string<))))

(defun by-dict__ (value ty)
    (let ((v-strs (mapcar (lambda (kv) (concatenate 'string (val-to-s__ (car kv) (slot-value ty 'key-type)) "=>" (val-to-s__ (cdr kv) (slot-value ty 'value-type)))) (alexandria:hash-table-alist value))))
        (format nil "{~{~A~^, ~}}" (sort v-strs #'string<))))

(defun by-option__ (value ty)
    (if (null value)
        "null"
        (val-to-s__ value (slot-value ty 'value-type))))

(defun val-to-s__ (value ty)
    (let ((type-name (slot-value ty 'type-name)))
        (cond ((string= type-name "bool") (if (not (or (eq value t) (eq value nil)))
                                            (error "Type mismatch")
                                            (by-bool__ value)))
              ((string= type-name "int") (if (not (integerp value))
                                            (error "Type mismatch")
                                            (by-int__ value)))
              ((string= type-name "double") (if (not (floatp value))
                                            (error "Type mismatch")
                                            (by-double__ value)))
              ((string= type-name "str") (if (not (stringp value))
                                            (error "Type mismatch")
                                            (by-string__ value)))
              ((string= type-name "list") (if (not (listp value))
                                            (error "Type mismatch")
                                            (by-list__ value ty)))
              ((string= type-name "ulist") (if (not (listp value))
                                            (error "Type mismatch")
                                            (by-ulist__ value ty)))
              ((string= type-name "dict") (if (not (hash-table-p value))
                                            (error "Type mismatch")
                                            (by-dict__ value ty)))
              ((string= type-name "option") (by-option__ value ty))
              (t (error (concatenate 'string "Unknown type " type-name))))))

(defun stringify__ (value type-str)
    (concatenate 'string (val-to-s__ value (s-to-type__ type-str)) ":" type-str))

(let ((tfs (concatenate 'string 
            (stringify__ t "bool") "
"
            (stringify__ 3 "int") "
"
            (stringify__ 3.141592653 "double") "
"
            (stringify__ 3.0 "double") "
"
            (stringify__ "Hello, World!" "str") "
"
            (stringify__ "!@#$%^&*()\\\"
	" "str") "
"
            (stringify__ (list 1 2 3) "list<int>") "
"
            (stringify__ (list t nil t) "list<bool>") "
"
            (stringify__ (list 3 2 1) "ulist<int>") "
"
            (stringify__ (alexandria:alist-hash-table (list (cons 1 "one") (cons 2 "two"))) "dict<int,str>") "
"
            (stringify__ (alexandria:alist-hash-table (list (cons "one" (list 1 2 3)) (cons "two" (list 4 5 6)))) "dict<str,list<int>>") "
"
            (stringify__ nil "option<int>") "
"
            (stringify__ 3 "option<int>") "
"    
            )))
    (with-open-file (stream "stringify.out" :direction :output :if-exists :supersede)
        (format stream tfs)))