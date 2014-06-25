(in-package :weblocks-filtering-widget)

(defwidget filtering-form (quickform)
  ((filtering-widget-instance)))

(defclass filtering-data ()
  ((field :initarg :field) 
   (compare-type :initarg :compare-type)
   (compare-value :initform nil)))

(defun filtering-form-view-field-wt (&key label-class id show-required-indicator required-indicator-label 
                               show-field-label field-label validation-error content 
                               field-class)
  (with-html-to-string
    (:li :class field-class
     (:label :class label-class
      :style "display:block;float:left;width:100px;"
      :for id
      (:span :class "slot-name"
       (:span :class "extra"
        (when show-field-label 
          (str field-label)
          (str ":&nbsp;")))))
     (:div :style "float:left;width:200px;"
      (str content))
     (when validation-error
       (htm (:p :class "validation-error"
             (:em
               (:span :class "validation-error-heading" "Error:&nbsp;")
               (str validation-error)))))
     (:div :style "clear:both"))))

(deftemplate :form-view-field-wt 'filtering-form-view-field-wt 
             :context-matches (lambda (&rest args &key widget &allow-other-keys)
                                (if (subtypep (type-of widget) 'filtering-form)
                                  20
                                  0)))

(defun filtering-form-view-field-for-bootstrap-wt (&key label-class id show-required-indicator required-indicator-label 
                                                        show-field-label field-label validation-error content 
                                                        field-class)
  (with-html-to-string
    (:style :type "text/css"
            (str "
                 div.submit {
                 margin-top: 10px;
                 }

                 .filtering-data .control-group {
                 margin-bottom: 0;
                 }

                 .filtering-data input, .filtering-data .value, .filtering-data select {
                 margin-left: 25px;
                 }

                 .filtering-form .compare-value input {
                 width: 550px;
                 }

                 .filtering-form .compare-value input.input-small {
                 width: 90px;
                 }
                 "))
    (:div :class (format nil "control-group ~A" field-class)
     (:label :class (format nil "control-label ~A" label-class)
      :style "display:block;float:left;"
      :for id
      (:span :class "slot-name"
       (:span :class "extra"
        (when show-field-label 
          (str field-label)
          (str ":&nbsp;")))))
     (:div 
       (str content))
     (when validation-error
       (htm (:p :class "validation-error"
             (:em
               (:span :class "validation-error-heading" "Error:&nbsp;")
               (str validation-error)))))
     (:div :style "clear:both"))))

(when (find-package :weblocks-twitter-bootstrap-application)
  (deftemplate :form-view-field-wt 'filtering-form-view-field-for-bootstrap-wt 
               :application-class (intern "TWITTER-BOOTSTRAP-WEBAPP" "WEBLOCKS-TWITTER-BOOTSTRAP-APPLICATION")
               :context-matches (lambda (&rest args &key widget &allow-other-keys)
                                  (if (subtypep (type-of widget) 'filtering-form)
                                    20
                                    0))))

(defun make-filtering-form (widget data &rest args)
  (let* ((presentation (cl-config:get-value 
                         :weblocks-filtering-widget.filtering-form-fields-presentation 
                         :default 'links-choices))
         (view (eval `(defview nil 
                               (:type filtering-form :persistp nil :buttons '((:submit . "Search") (:cancel . "Cancel")) 
                                :caption ,(filtering-widget-form-title widget))
                               (field :label ,(cl-config:get-value 
                                                :weblocks-filtering-widget.filtering-form-field-caption 
                                                :default "Search for ...") :present-as (,presentation :choices ',(compare-field-form-choices widget))
                                      :requiredp t)
                               (compare-type 
                                 :label ,(cl-config:get-value 
                                           :weblocks-filtering-widget.filtering-form-compare-type-caption
                                           :default "which ..")
                                 :present-as 
                                 (,presentation :choices '(("is like ..." . "like") 
                                                           ("is equal to ..." . "equal")
                                                           ("is not like ..." . "not-like")
                                                           ("is not equal to ..." . "not-equal")
                                                           ("is more ..." . "greater-number")
                                                           ("is less ..." . "less-number")
                                                           ("is later ..." . "greater-date")
                                                           ("is earlier ..." . "less-date")
                                                           ("is identical ..." . "identical")
                                                           ("is not identical ..." . "not-identical")
                                                           ))
                                 :requiredp t)
                               (compare-value :label 
                                              ,(cl-config:get-value 
                                                 :weblocks-filtering-widget.filtering-form-compare-value-caption
                                                 :default "value ...") :present-as input))))
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
      (setf (slot-value (dataform-form-view form) 'form) form)
      form)))
