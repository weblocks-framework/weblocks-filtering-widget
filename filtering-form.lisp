(in-package :weblocks-filtering-widget)

(defwidget filtering-form (quickform)
  ((filtering-widget-instance)))

(defun make-filtering-form (widget &rest args)
  (let* ((view (eval `(defview nil 
                               (:type form :persistp nil :buttons '((:submit . "Search") (:cancel . "Cancel")) 
                                :caption ,(filtering-widget-form-title widget))
                               (field :present-as (dropdown :choices ',(compare-field-form-choices widget)) :requiredp t)
                               (compare-type :present-as (dropdown :choices '("equal" "like")) :requiredp t)
                               (compare-value :present-as input))))
         (form (apply #'make-quickform 
                      view
                      (append 
                        (list 
                          :class 'filtering-form 
                          :answerp nil)
                        args))))

    (with-slots (filtering-widget-instance) form
      (setf filtering-widget-instance widget)
      form)))
