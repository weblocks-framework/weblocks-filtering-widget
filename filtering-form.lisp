(in-package :weblocks-filtering-widget)

(defwidget filtering-form (quickform)
  ((filtering-widget-instance)))

(defclass filtering-data ()
  ((field :initarg :field) 
   (compare-type :initarg :compare-type) compare-value))

(defun make-filtering-form (widget data &rest args)
  (let* ((view (eval `(defview nil 
                               (:type form :persistp nil :buttons '((:submit . "Search") (:cancel . "Cancel")) 
                                      :caption ,(filtering-widget-form-title widget))
                               (field :present-as (dropdown :choices ',(compare-field-form-choices widget)) :requiredp t)
                               (compare-type 
                                 :present-as 
                                 (dropdown :choices '(("Is like ..." . "like") 
                                                      ("Is equal to ..." . "equal")
                                                      ("Is not like ..." . "not-like")
                                                      ("Is not equal to ..." . "not-equal"))) :requiredp t)
                               (compare-value :present-as input))))
         (data (apply #'make-instance (list* 'filtering-data data)))
         (form 
           (progn 
             (apply #'make-quickform 
                    view
                    (append 
                      (list 
                        :data data
                        :class 'filtering-form 
                        :answerp nil)
                      args)))))

    (with-slots (filtering-widget-instance) form
      (setf filtering-widget-instance widget)
      form)))
