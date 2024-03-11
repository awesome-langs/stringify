(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-system :alexandria)
(defstruct poly-eval-type type-str type-name value-type key-type)

(defun __new-poly-eval-type (type-str type-name value-type key-type)
    (make-poly-eval-type :type-str type-str :type-name type-name :value-type value-type :key-type key-type))

(defun __s-to-type (type-str)
    (if (not (search "<" type-str))
        (__new-poly-eval-type type-str type-str nil nil)
        (let* ((idx (position #\< type-str))
              (type-name (subseq type-str 0 idx))
              (other-str (subseq type-str (+ 1 idx) (- (length type-str) 1))))
            (if (not (search "," other-str))
                (let ((value-type (__s-to-type other-str)))
                    (__new-poly-eval-type type-str type-name value-type nil))
                (let* ((idx (position #\, other-str))
                      (key-type (__s-to-type (subseq other-str 0 idx)))
                      (value-type (__s-to-type (subseq other-str (+ 1 idx)))))
                    (__new-poly-eval-type type-str type-name value-type key-type))))))

(defun __escape-string (s)
    (let ((new-s (mapcar (lambda (c)
        (cond ((char= c #\\) "\\\\")
              ((char= c #\") "\\\"")
              ((char= c #\newline) "\\n")
              ((char= c #\tab) "\\t")
              ((char= c #\return) "\\r")
              (t (string c)))) (coerce s 'list))))
        (apply #'concatenate 'string new-s)))

(defun __by-bool (value)
    (if value "true" "false"))


(defun __by-int (value)
    (write-to-string (truncate value)))


(defun __by-double (value)
    (let ((vs (format nil "~,6f" value)))
        (loop while (string= (subseq vs (- (length vs) 1)) "0") do
            (setf vs (subseq vs 0 (- (length vs) 1))))
        (if (string= (subseq vs (- (length vs) 1)) ".")
            (setf vs (concatenate 'string vs "0"))
            (if (string= vs "-0.0")
                (setf vs "0.0")))
        vs))

(defun __by-string (value)
    (concatenate 'string "\"" (__escape-string value) "\""))

(defun __by-list (value ty)
    (let ((v-strs (mapcar (lambda (v) (__val-to-s v (slot-value ty 'value-type))) value)))
        (format nil "[~{~A~^, ~}]" v-strs)))

(defun __by-ulist (value ty)
    (let ((v-strs (mapcar (lambda (v) (__val-to-s v (slot-value ty 'value-type))) value)))
        (format nil "[~{~A~^, ~}]" (sort v-strs #'string<))))

(defun __by-dict (value ty)
    (let ((v-strs (mapcar (lambda (kv) (concatenate 'string (__val-to-s (car kv) (slot-value ty 'key-type)) "=>" (__val-to-s (cdr kv) (slot-value ty 'value-type)))) (alexandria:hash-table-alist value))))
        (format nil "{~{~A~^, ~}}" (sort v-strs #'string<))))

(defun __by-option (value ty)
    (if (null value)
        "null"
        (__val-to-s value (slot-value ty 'value-type))))

(defun __val-to-s (value ty)
    (let ((type-name (slot-value ty 'type-name)))
        (cond ((string= type-name "bool") (if (not (or (eq value t) (eq value nil)))
                                            (error "Type mismatch")
                                            (__by-bool value)))
              ((string= type-name "int") (if (not (integerp value))
                                            (error "Type mismatch")
                                            (__by-int value)))
              ((string= type-name "double") (if (not (floatp value))
                                            (error "Type mismatch")
                                            (__by-double value)))
              ((string= type-name "str") (if (not (stringp value))
                                            (error "Type mismatch")
                                            (__by-string value)))
              ((string= type-name "list") (if (not (listp value))
                                            (error "Type mismatch")
                                            (__by-list value ty)))
              ((string= type-name "ulist") (if (not (listp value))
                                            (error "Type mismatch")
                                            (__by-ulist value ty)))
              ((string= type-name "dict") (if (not (hash-table-p value))
                                            (error "Type mismatch")
                                            (__by-dict value ty)))
              ((string= type-name "option") (__by-option value ty))
              (t (error (concatenate 'string "Unknown type " type-name))))))

(defun __stringify (value type-str)
    (concatenate 'string (__val-to-s value (__s-to-type type-str)) ":" type-str))

(let ((tfs (concatenate 'string 
            (__stringify t "bool") "
"
            (__stringify 3 "int") "
"
            (__stringify 3.141592653 "double") "
"
            (__stringify 3.0 "double") "
"
            (__stringify "Hello, World!" "str") "
"
            (__stringify "!@#$%^&*()\\\"
	" "str") "
"
            (__stringify (list 1 2 3) "list<int>") "
"
            (__stringify (list t nil t) "list<bool>") "
"
            (__stringify (list 3 2 1) "ulist<int>") "
"
            (__stringify (alexandria:alist-hash-table (list (cons 1 "one") (cons 2 "two"))) "dict<int,str>") "
"
            (__stringify (alexandria:alist-hash-table (list (cons "one" (list 1 2 3)) (cons "two" (list 4 5 6)))) "dict<str,list<int>>") "
"
            (__stringify nil "option<int>") "
"
            (__stringify 3 "option<int>") "
"    
            )))
    (with-open-file (stream "stringify.out" :direction :output :if-exists :supersede)
        (format stream tfs)))