(in-package :weblocks-filtering-widget)

(defun like-expression->regexp (expression)
  (let ((parts (cl-ppcre:split "\\*" expression)))
    (format nil ".*~{~A.*~}" (mapcar #'cl-ppcre:quote-meta-chars parts))))

(defun string-like-pattern-p (value1 value2)
  (if (cl-ppcre:scan (like-expression->regexp value2) value1) t))

(defun string-not-like-pattern-p (value1 value2)
  (not (string-like-pattern-p value1 value2)))

(defun string-not= (str1 str2)
  (not (string= str1 str2)))

(defvar *compare-functions* (list 
                              :equal #'string=
                              :like #'string-like-pattern-p 
                              :not-equal #'string-not=
                              :not-like #'string-not-like-pattern-p))
(defvar *accessors*)

(defun compare-single-value (filter-value model-instance &optional (compare-functions *compare-functions*))
  (declare (special *accessors*))
  (let* ((compare-function-key (intern (string-upcase (getf filter-value :compare-type)) "KEYWORD"))
         (compare-func (or (getf compare-functions compare-function-key)
                           (error (format nil "Function ~A not found in compare-functions" compare-function-key))))
         (filter-accessor (getf *accessors* (getf filter-value :field)))
         (return 
           (if filter-accessor
             (funcall 
               compare-func
               (handler-case 
                 (let ((value (funcall filter-accessor model-instance))) 
                   (if (stringp value)
                     value
                     (write-to-string value)))
                 (unbound-slot () nil)) 
               (getf filter-value :compare-value))
             nil)))
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
