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
                 (funcall filter-accessor model-instance)
                 (unbound-slot () nil)) 
               (getf filter-value :compare-value))
             nil)))
    return)) 

(defun compare-or (filters model-instance &optional default)
  (let ((return 
          (if filters 
            (or 
              (compare-or (getf filters :or) model-instance default)
              (if (compare-single-value (getf filters :value) model-instance)
                (compare-and (getf filters :and) model-instance t)
                (compare-and (getf filters :and) model-instance nil)))
            default)))
    return))

(defun compare-and (filters model-instance &optional default)
  (let ((return 
          (if filters
            (and 
              (compare-and (getf filters :and) model-instance t)
              (if (compare-single-value (getf filters :value) model-instance)
                (compare-or (getf filters :or) model-instance t)
                (compare-or (getf filters :or) model-instance nil)))
            default))) 
    return))

(defun compare (filters model-instance &optional (accessors *accessors*))
  (let ((*accessors* accessors))
    (let ((return (compare-and filters model-instance t))) 
      return)))

(defclass test-item ()
  ((name :accessor test-item-name :initarg :name)
   (age :accessor test-item-age :initarg :age)
   (some-other-attr :accessor some-other-attr :initarg :attr)))

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
              :and (:value (:field :age :compare-type "equal" :compare-value "Ice age"))
              :or nil) 
            (make-instance 'test-item :name "Test" :age "Ice age"))) 

  ; (and t nil)
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test" :compare-type "equal")
                   :and (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
                 (make-instance 'test-item :name "Test" :age "Ice age")))) 

  ; (or t nil)
  (assert (compare 
            '(:value (:field :name :compare-value "Test" :compare-type "equal")
              :or (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
            (make-instance 'test-item :name "Test" :age "Ice age"))) 

  ; (or nil t)
  (assert (compare 
            '(:value (:field :name :compare-value "Test-2" :compare-type "equal")
              :or (:value (:field :age :compare-type "equal" :compare-value "Ice age"))) 
            (make-instance 'test-item :name "Test" :age "Ice age")))

  ; (or nil nil) XXX
  
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                   :or (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
                 (make-instance 'test-item :name "Test" :age "Ice age"))))

  ; (and nil nil)
  (assert (not (compare 
                 '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                   :and (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
                 (make-instance 'test-item :name "Test" :age "Ice age")))))

