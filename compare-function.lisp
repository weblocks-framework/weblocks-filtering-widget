(in-package :weblocks-filtering-widget)

(defun like-expression->regexp (expression)
  (let ((parts (cl-ppcre:split "\\*" expression)))
    (format nil ".*~{~A.*~}" (mapcar #'cl-ppcre:quote-meta-chars parts))))

(defun string-like-pattern-p (value1 value2 &key (case-sensitive t))
  (if (cl-ppcre:scan (cl-ppcre:create-scanner 
                       (like-expression->regexp value2)
                       :case-insensitive-mode t) value1) t))

(defun string-not-like-pattern-p (value1 value2)
  (not (string-like-pattern-p value1 value2)))

(defun string-not= (str1 str2)
  (not (string= str1 str2)))

(defun case-insensitive-string= (str1 str2)
  (string= (string-downcase str1) (string-downcase str2)))

(assert (case-insensitive-string= "Test" "test"))

(defun case-insensitive-string-not= (str1 str2)
  (not (case-insensitive-string= str1 str2)))

(assert (not (case-insensitive-string-not= "Test" "test")))

(defun case-insensitive-string-like-pattern-p (str1 str2)
  (string-like-pattern-p str1 str2 :case-sensitive nil))

(defun case-insensitive-string-not-like-pattern-p (str1 str2)
  (not (case-insensitive-string-like-pattern-p str1 str2)))

(defun dirty-integerp (number)
  (or (integerp number)
      (ppcre:scan "\\d+" number)))

(defmethod force-string (obj)
  (cond 
    ((integerp obj) (write-to-string obj))
    ((stringp obj) obj)
    ((null obj) "")
    (t (error "Don't know how to force string format for ~A. Please define method weblocks-filtering-widget:force-string for this object." obj))))

(defun force-integer (string-or-number)
  (cond 
    ((numberp string-or-number) string-or-number)
    ((null string-or-number) 0)
    (t (parse-integer string-or-number))))

(defun safe>= (value1 value2)
  (>= (force-integer value1) (force-integer value2)))

(defun safe<= (value1 value2)
  (<= (force-integer value1) (force-integer value2)))

(defun safe> (value1 value2)
  (> (force-integer value1) (force-integer value2)))

(defun safe< (value1 value2)
  (< (force-integer value1) (force-integer value2)))

(defun item-in-list-p (item list)
  (find (force-string item) 
        (mapcar #'force-string list)
        :test #'string= ))

(defun objects-identical-p (item1 item2)
  (when item1 
    (cond 
      ((integerp item1) (= item1 (force-integer item2)))
      ((stringp item1) (string= item1 (force-string item2)))
      ((and (dirty-integerp item2) (integerp (ignore-errors (object-id item1))))
       (= (force-integer item2) (force-integer (object-id item1))))
      (t (progn 
           (error "Identity is not supported for objects ~A ~A of types ~A ~A~%" item1 item2 (type-of item1) (type-of item2)))))))

(defun objects-not-identical-p (item1 item2)
  (not (objects-identical-p item1 item2)))

(defun null-p (item1 item2)
  (null item1))

(defun not-null-p (item1 item2)
  (not (null item1)))

(defvar *compare-functions* (list 
                              :case-sensitive 
                              (list 
                                :equal #'string=
                                :like #'string-like-pattern-p 
                                :not-equal #'string-not=
                                :not-like #'string-not-like-pattern-p)
                              :case-insensitive 
                              (list 
                                :equal #'case-insensitive-string=
                                :like #'case-insensitive-string-like-pattern-p
                                :not-equal #'case-insensitive-string-not=
                                :not-like #'case-insensitive-string-not-like-pattern-p)
                              :numbers 
                              (list 
                                :greater-or-equal-number #'safe>=
                                :less-or-equal-number #'safe<=
                                :greater-number #'safe>
                                :less-number #'safe<)
                              :dates 
                              (list 
                                :greater-date #'safe>=
                                :less-date #'safe<=)
                              :objects 
                              (list 
                                :null #'null-p
                                :not-null #'not-null-p
                                :identical #'objects-identical-p
                                :not-identical #'objects-not-identical-p)
                              :lists 
                              (list 
                                :in 'item-in-list-p)))
(defvar *accessors*)

(defun compare-single-value (filter-value model-instance &optional (compare-functions (append 
                                                                                        (getf *compare-functions* :case-insensitive)
                                                                                        (getf *compare-functions* :numbers)
                                                                                        (getf *compare-functions* :dates)
                                                                                        (getf *compare-functions* :lists)
                                                                                        (getf *compare-functions* :objects)
                                                                                        )))
  (declare (special *accessors*))
  (let* ((compare-function-key (intern (string-upcase (getf filter-value :compare-type)) "KEYWORD"))
         (compare-func (cond 
                         ((functionp (getf filter-value :compare-function)) 
                          (getf filter-value :compare-function))
                         (t (or (getf compare-functions compare-function-key)
                                (error (format nil "Function ~A not found in compare-functions" compare-function-key))))))
         (filter-accessor (getf *accessors* (getf filter-value :field)))
         (comparing-numbers-p 
           (getf 
             (getf *compare-functions* :numbers) 
             compare-function-key))
         (comparing-strings-p 
           (getf 
             (getf *compare-functions* :case-insensitive) 
             compare-function-key))
         (return 
           (if filter-accessor
             (funcall 
               compare-func
               (handler-case 
                 (let ((value (funcall filter-accessor model-instance))) 
                   (cond 
                     (comparing-numbers-p value)
                     (comparing-strings-p 
                       (force-string value))
                     (t value)))
                 (unbound-slot () nil)) 
               (getf filter-value :compare-value))
             (error "No filter accessor for ~A" (getf filter-value :field)))))
    return))


(defclass test-item ()
  ((name :accessor test-item-name :initarg :name)
   (age :accessor test-item-age :initarg :age)
   (some-other-attr :accessor some-other-attr :initarg :attr)))

(defun map-filters-c (callback filters)
  (loop for i in filters collect
        (progn 
          (if (listp i) 
            (progn
              (when (listp (getf i :and))
                (setf (getf i :and)
                      (map-filters-c callback (getf i :and)))) 
              (when (listp (getf i :or))
                (setf (getf i :or)
                      (map-filters-c callback (getf i :or)))) 
              (funcall callback i))
            i))))

(defun values-descriptions-to-values-map (item model-instance)
  (when (and (getf item :value) (listp (getf item :value)))
    (setf (getf item :value)
        (compare-single-value (getf item :value) model-instance)))
  item)

(defun values-descriptions-to-values (filters model-instance)
  (map-filters-c 
    (lambda (item)
      (values-descriptions-to-values-map item model-instance)) 
    (list filters)))

(defun empty-expressions-to-true (filters)
  (map-filters-c 
    (lambda (item)
      (unless (getf item :or)
        (setf (getf item :or) nil))
      (unless (getf item :and)
        (setf (getf item :and) t))
      item)
    filters))

(defun list-with-non-zero-length-p (list)
  (and list (listp list) (not (zerop (length list)))))

(defun expressions-to-boolean (filters)
  (map-filters-c
    (lambda (item)
      (if (or 
            (list-with-non-zero-length-p 
              (getf item :or))
            (list-with-non-zero-length-p 
              (getf item :and)))
        item
        (progn 
          (and 
            (getf item :and)
            (or (getf item :value)
                (getf item :or))))))
    filters))

(defun is-expression-to-be-shorted (filter)
  (if (or (not (listp filter)) (some #'list-with-non-zero-length-p filter))
    nil
    t))

(defun single-short-and-or-expressions (filter)
  (if (is-expression-to-be-shorted (getf filter :and))
    (setf (getf filter :and) (eval `(and ,@(getf filter :and))))
    (setf (getf filter :and) (short-and-or-expressions (getf filter :and))))
  (if (is-expression-to-be-shorted (getf filter :or))
    (setf (getf filter :or) (eval `(or ,@(getf filter :or))))
    (setf (getf filter :or) (short-and-or-expressions (getf filter :or))))
  filter)

(defun short-and-or-expressions (filters)
  (if (listp filters)
    (loop for i in filters 
        collect (if 
                  (listp i) 
                  (single-short-and-or-expressions i) i))
    filters))

(defun compare (filters model-instance &optional (accessors *accessors*))
  (declare (special *accessors*))
  (if filters 
    (let ((*accessors* accessors)
          (data (copy-tree filters)))
      (setf data (values-descriptions-to-values data model-instance))
      (setf data (empty-expressions-to-true data))
      (loop do 
            (setf data (expressions-to-boolean data))
            (when (is-expression-to-be-shorted data)
              (return-from compare (eval `(and ,@data))))
            (setf data (short-and-or-expressions data))))
    t))

(let ((*accessors* (list :name #'test-item-name :age #'test-item-age :attr #'some-other-attr)))
  ; t
  (assert (compare nil (make-instance 'test-item :name "Test"))) 

  ; t
  (assert (compare (list :value (list :field :name :compare-value "Test" :compare-type "equal") :and nil :or nil) (make-instance 'test-item :name "Test"))) 

  ; nil
  (assert (not (compare 
                 (list :value (list :field :name :compare-value "Test-2" :compare-type "equal")) 
                 (make-instance 'test-item :name "Test")))) 

  ; (and t t)
  (assert (compare 
            '(:value (:field :name :compare-value "Test" :compare-type "equal")
              :and ((:value (:field :age :compare-type "equal" :compare-value "Ice age")))
              :or nil) 
            (make-instance 'test-item :name "Test" :age "Ice age"))) 

  ; (and t nil)
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test" :compare-type "equal")
                   :and ((:value (:field :age :compare-type "equal" :compare-value "Modern age")))) 
                 (make-instance 'test-item :name "Test" :age "Ice age")))) 

  ; (or t nil)
  (assert (compare 
            '(:value (:field :name :compare-value "Test" :compare-type "equal")
              :or ((:value (:field :age :compare-type "equal" :compare-value "Modern age") :and nil :or nil))
              :and nil) 
            (make-instance 'test-item :name "Test" :age "Ice age"))) 


  ; (or nil t)
  (assert (compare 
            '(:value (:field :name :compare-value "Test-2" :compare-type "equal")
              :or ((:value (:field :age :compare-type "equal" :compare-value "Ice age") :and nil :or nil))
              :and nil) 
            (make-instance 'test-item :name "Test" :age "Ice age")))


  ; (or nil nil) XXX
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                   :or ((:value (:field :age :compare-type "equal" :compare-value "Modern age") :or nil :and nil))
                   :and nil) 
                 (make-instance 'test-item :name "Test" :age "Ice age"))))

  ; (and nil nil)
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                   :and ((:value (:field :age :compare-type "equal" :compare-value "Modern age") :and nil :or nil))
                   :or nil) 
                 (make-instance 'test-item :name "Test" :age "Ice age")))) 

  ; Just checking this expression works
  (assert (not (compare '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                          :and nil
                          :or ((:value (:field :age :compare-type "equal" :compare-value "Modern age") 
                                :and ((:value (:field :age :compare-type "equal" :compare-value "Modern age 2") 
                                       :or nil
                                       :and nil)) 
                                :or nil))) (make-instance 'test-item :name "Test" :age "Ice age")))) 

  ; Just checking this expression works
  (assert (compare '(:VALUE T :ID "#:G1765" :AND T :OR
                          ((:VALUE T :ID "#:G1873" :AND T :OR NIL)
                           (:VALUE T :ID "#:G1792" :AND ((:VALUE T :ID "#:G1846" :AND T :OR NIL) (:VALUE T :ID "#:G1819" :AND T :OR NIL)) :OR NIL))) (make-instance 'test-item :name "Test" :age "Ice age"))) 

  ; Just checking this expression works
  (assert (compare '(:VALUE T :ID "#:G1590" :AND T :OR ((:VALUE T :ID "#:G1672" :AND (T (:VALUE T :ID "#:G1715" :AND T :OR (T))) :OR NIL))) (make-instance 'test-item :name "Test" :age "Ice age")))) 
