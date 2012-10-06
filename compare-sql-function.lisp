
(in-package :weblocks-filtering-widget)

(defun transform-like-expression-wildcards (string)
  (flet ((surround-with-percents-sign (string)
           (concatenate 'string "%" string "%"))
         (percent-signs-to-double-signs (string)
           (ppcre:regex-replace "%" string "%%"))
         (wildcards-to-percent-signs (string)
           (ppcre:regex-replace "\\*"  string "%")))
    (surround-with-percents-sign
      (wildcards-to-percent-signs 
        (percent-signs-to-double-signs string)))))

(assert (string= "%test%" (transform-like-expression-wildcards "test")))
(assert (string= "%te%st%" (transform-like-expression-wildcards "te*st")))
(assert (string= "%te%%st%" (transform-like-expression-wildcards "te%st")))

(defun compare-sql-expression (filters)
  (let ((value (getf filters :value)))
    (when (getf value :compare-type)
      (let* ((is-a-not-expression (string= 
                                    "not-"
                                    (subseq (getf value :compare-type) 0 4)))
             (string-expression (if is-a-not-expression 
                                  (subseq (getf value :compare-type) 4) 
                                  (getf value :compare-type)))
             (operator 
               (cond 
                 ((string= string-expression "equal") '=)
                 ((string= string-expression "like") 'like)
                 (t (error (format nil "Operation \"~A\" is not implemented" (getf value :compare-type))))))
             (sub-expressions 
               (cond 
                 ((string= string-expression "equal") 
                  (list (intern (string (getf value :field))) (getf value :compare-value)))
                 ((string= string-expression "like")
                  (list (intern (string (getf value :field))) 
                        (transform-like-expression-wildcards 
                          (getf value :compare-value))))))
             (expression
               (make-instance 
                 (intern "SQL-RELATIONAL-EXP" "CLSQL-SYS")
                 :operator operator
                 :sub-expressions sub-expressions)))
        (when is-a-not-expression 
          (setf expression 
                (make-instance 
                  (intern "SQL-VALUE-EXP" "CLSQL-SYS")
                  :modifier 'not 
                  :components 
                  (list 
                    expression))))
        expression))))

(defun assert-expressions-equal (expression1 expression2)
  (assert (string= 
            (write-to-string expression1)
            (write-to-string expression2))))

(when (find-package :clsql)
  (eval `(,(intern "LOCALLY-ENABLE-SQL-READER-SYNTAX" "CLSQL")))

  (assert-expressions-equal 
    [= 'post_subject "test"]
    (compare-sql-expression '(:VALUE (:FIELD :POST_SUBJECT :COMPARE-TYPE "equal" :COMPARE-VALUE "test") :ID "#:G1966" :AND NIL :OR NIL)))

  (assert-expressions-equal 
    [not [= 'post_subject "test"]]
    (compare-sql-expression '(:VALUE (:FIELD :POST_SUBJECT :COMPARE-TYPE "not-equal" :COMPARE-VALUE "test") :ID "#:G1966" :AND NIL :OR NIL)))

  (assert-expressions-equal 
    [like 'post_subject "%test%"]
    (compare-sql-expression '(:VALUE (:FIELD :POST_SUBJECT :COMPARE-TYPE "like" :COMPARE-VALUE "test") :ID "#:G1966" :AND NIL :OR NIL)))

  (assert-expressions-equal 
    [not [like 'post_subject "%test%"]]
    (compare-sql-expression '(:VALUE (:FIELD :POST_SUBJECT :COMPARE-TYPE "not-like" :COMPARE-VALUE "test") :ID "#:G1966" :AND NIL :OR NIL)))

  (eval `(,(intern "RESTORE-SQL-READER-SYNTAX-STATE" "CLSQL"))))

