(in-package :weblocks-filtering-widget)

(defwidget filtering-form (quickform)
  ((filtering-widget-instance)))

(defclass filtering-data ()
  ((field :initarg :field) 
   (compare-type :initarg :compare-type)
   (compare-value)))

(defclass links-choices-presentation (form-presentation choices-presentation-mixin)
  ())

(defmethod render-view-field-value (value (presentation links-choices-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (ignore args)
	   (special *presentation-dom-id*))
  (multiple-value-bind (intermediate-value intermediate-value-p)
    (form-field-intermediate-value field intermediate-values)
    (let* ((attributized-slot-name 
             (if field-info
               (attributize-view-field-name field-info)
               (attributize-name (view-field-slot-name field))))
           (key (intern (string-upcase attributized-slot-name) "WEBLOCKS-FILTERING-WIDGET")))
      (with-html 
        (:input :type "hidden" :name attributized-slot-name :value value)
        (:ul :style "display:inline-block"
         (loop for i in (obtain-presentation-choices presentation obj) for j from 0 do 
               (htm 
                 (:li :style "display:block;float:left;border:0;padding:0 3px;"
                  (if (or (and (not (slot-value obj key)) (zerop j)) 
                          (string= (string (slot-value obj key)) (cdr i)))
                    (htm (:b (str (car i))))
                    (let ((action 
                            (function-or-action->action 
                              (lambda (&rest args) 
                                (let ((slot (intern (string-upcase attributized-slot-name) "WEBLOCKS-FILTERING-WIDGET"))
                                      (key (intern (string-upcase attributized-slot-name) "KEYWORD")))
                                  (setf (slot-value obj slot) (getf args key))
                                  (if (not intermediate-values) 
                                    (setf (slot-value widget 'weblocks::intermediate-form-values)
                                        (apply #'weblocks::request-parameters-for-object-view
                                               view (list key (intern (getf args key) "KEYWORD"))))
                                    (setf (cdr (assoc field (slot-value widget 'weblocks::intermediate-form-values)))
                                          (intern (getf args key) "KEYWORD")))
                                  (mark-dirty widget))))))
                      (htm (:a :href (add-get-param-to-url (make-action-url action) attributized-slot-name (cdr i)) 
                            :onclick (format nil 
                                             "initiateActionWithArgs(\"~A\", \"~A\", {\"~A\": \"~A\"});return false;"
                                             action (session-name-string-pair) attributized-slot-name (cdr i))
                            (str (car i))))))))))))))


; Small fix, copied from weblocks/src/views/formview/formview.lisp
(defmethod render-view-field ((field form-view-field) (view form-view)
                                                      (widget filtering-form) presentation value obj 
                                                      &rest args &key validation-errors field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let* ((attributized-slot-name (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name (view-field-slot-name field))))
         (validation-error (assoc field validation-errors))
         (field-class (concatenate 'string (aif attributized-slot-name it "")
                                   (when validation-error " item-not-validated")))
         (*presentation-dom-id* (gen-id)))
    (with-html
      (:li :class field-class
       (:label :class (attributize-presentation
                        (view-field-presentation field))
               :style "display:block;float:left;width:100px;"
               :for *presentation-dom-id*
               (:span :class "slot-name"
                (:span :class "extra"
                 (unless (empty-p (view-field-label field))
                   (str (view-field-label field)))
                 (let ((required-indicator nil))
                   (when (and (form-view-field-required-p field)
                              required-indicator)
                     (htm (:em :class "required-slot"
                           (if (eq t required-indicator)
                             (str *default-required-indicator*)
                             (str required-indicator))
                           (str "&nbsp;"))))))))
       (:div 
         :style "float:left;width:200px;"
         (apply #'render-view-field-value
              value presentation
              field view widget obj
              :field-info field-info
              args))
       (when validation-error
         (htm (:p :class "validation-error"
               (:em
                 (:span :class "validation-error-heading" "Error:&nbsp;")
                 (str (format nil "~A" (cdr validation-error)))))))
       (:div :style "clear:both")))))

(defun make-filtering-form (widget data &rest args)
  (let* ((presentation (cl-config:get-value 
                         :weblocks-filtering-widget.filtering-form-fields-presentation 
                         :default 'links-choices))
         (view (eval `(defview nil 
                               (:type form :persistp nil :buttons '((:submit . "Search") (:cancel . "Cancel")) 
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
                                                           ("is not equal to ..." . "not-equal")))
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
      form)))
