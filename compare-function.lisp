(in-package :weblocks-filtering-widget)

(defvar *accessors*)

(defun compare-single-value (filter-value model-instance)
  (declare (special *accessors*))
  (let* ((compare-function #'string=)
         (filter-accessor (getf *accessors* (getf filter-value :field)))
         (return 
           (if filter-accessor
             (funcall 
               compare-function
               (funcall filter-accessor model-instance)
               (getf filter-value :compare-value))
             nil)))
    return)) 

(defun compare-or (filters model-instance)
  (let ((return 
          (if filters 
            (or (compare-single-value (getf filters :value) model-instance)
                (compare-and (getf filters :and) model-instance))
            t)))
    return))

(defun compare-and (filters model-instance)
  (let ((return 
          (if filters
            (and 
              (compare-single-value (getf filters :value) model-instance)
              (compare-and (getf filters :and) model-instance)
              (compare-or (getf filters :or) model-instance))
            t))) 
    return))

(defun compare (filters model-instance &optional (accessors *accessors*))
  (let ((*accessors* accessors))
    (let ((return (compare-and filters model-instance))) 
      return)))

(defclass test-item ()
  ((name :accessor test-item-name :initarg :name)
   (age :accessor test-item-age :initarg :age)
   (some-other-attr :accessor some-other-attr :initarg :attr)))

(let ((*accessors* (list :name #'test-item-name :age #'test-item-age :attr #'some-other-attr)))
  (progn
    (assert (compare nil (make-instance 'test-item :name "Test"))) 
    (assert (compare (list :value (list :field :name :compare-value "Test") :and nil :or nil) (make-instance 'test-item :name "Test"))) 
    (assert (not (compare 
                   (list :value (list :field :name :compare-value "Test-2" :compare-type "equal")) 
                   (make-instance 'test-item :name "Test")))) 
    (assert (compare 
              '(:value (:field :name :compare-value "Test" :compare-type "equal")
                :and (:value (:field :age :compare-type "equal" :compare-value "Ice age"))
                :or nil) 
              (make-instance 'test-item :name "Test" :age "Ice age"))) 
    (assert (not (compare 
                   '(:value (:field :name :compare-value "Test" :compare-type "equal")
                     :and (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
                   (make-instance 'test-item :name "Test" :age "Ice age")))) 
    (assert (compare 
              '(:value (:field :name :compare-value "Test" :compare-type "equal")
                :or (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
              (make-instance 'test-item :name "Test" :age "Ice age"))))
  (assert (not 
            (compare 
              '(:value (:field :name :compare-value "Test 2" :compare-type "equal")
                :or (:value (:field :age :compare-type "equal" :compare-value "Modern age"))) 
              (make-instance 'test-item :name "Test" :age "Ice age")))))

