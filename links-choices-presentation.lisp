(in-package :weblocks-filtering-widget)

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
                                    (let ((field (assoc field (slot-value widget 'weblocks::intermediate-form-values))))
                                      (when field 
                                        (setf (cdr field)
                                              (intern (getf args key) "KEYWORD")))))
                                  (mark-dirty widget))))))
                      (htm (:a :href (add-get-param-to-url (make-action-url action) attributized-slot-name (cdr i)) 
                            :onclick (format nil 
                                             "initiateActionWithArgs(\"~A\", \"~A\", {\"~A\": \"~A\"});return false;"
                                             action (session-name-string-pair) attributized-slot-name (cdr i))
                            (str (car i))))))))))))))

