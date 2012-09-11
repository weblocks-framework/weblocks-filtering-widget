(in-package :weblocks-filtering-widget)

(defwidget filtering-widget (widget)
  ((filters :initform nil)
   (filter-form-visible :initform t)
   (filter-form-position :initform nil)
   (add-filter-action)
   (remove-filter-action)
   (form-title :initarg :form-title :initform "Filtering widget" :accessor filtering-widget-form-title)
   (form-fields :initarg :form-fields)
   (dataseq-instance :initarg :dataseq-instance :initform nil)))

(defun object->simple-plist (object &rest filters)
  (loop for i in (sb-mop:class-direct-slots (find-class (class-name  (class-of object)))) append 
        (let* ((slot (intern (string (sb-mop:slot-definition-name i)) "KEYWORD"))
               (value (if (slot-boundp object (sb-mop:slot-definition-name i))
                        (slot-value object (sb-mop:slot-definition-name i))
                        "Unbound")))
          (list slot (if (getf filters slot) (funcall (getf filters slot) value) value)))))

(defun get-css-border-radius (radius)
  (format nil "-webkit-border-radius: ~a; -moz-border-radius: ~a; border-radius: ~a;" radius radius radius))

(defun append-css-border-radius (radius str)
  (concatenate 'string (string-right-trim ";" str) ";" (get-css-border-radius radius)))

(defun map-filters (callback filters)
  (loop for i in filters collect
        (if (getf i :value)
          (progn 
            (setf (getf i :and)
                  (remove-if #'null (map-filters callback (getf i :and))))
            (setf (getf i :or)
                  (remove-if #'null (map-filters callback (getf i :or))))
            (funcall callback i))
          i)))

(defmethod filter-form-on-element-p ((widget filtering-widget) filter-id filter-type)
  (with-slots (filter-form-visible filter-form-position) widget 
    (let* ((id (car filter-form-position))
           (type (cdr filter-form-position))
           (is-needed-element (and 
                                (string= 
                                  id
                                  (string filter-id))
                                (string= 
                                  type
                                  (string filter-type)))))
      is-needed-element)))

(defmethod initialize-instance :around ((widget filtering-widget) &rest args)
  (with-slots (add-filter-action filters remove-filter-action) widget
    (setf add-filter-action (make-action 
                              (lambda (&rest args &key id &allow-other-keys)
                                (with-slots (filter-form-visible filter-form-position) widget 
                                  (setf filter-form-visible t)
                                  (if (getf args :id)
                                    (progn 
                                      (setf filter-form-position 
                                            (cons (getf args :id) (getf args :filter-type)))

                                      (mark-dirty widget)))))))
    (setf remove-filter-action (make-action 
                                 (lambda (&rest args)
                                   (with-slots (filter-form-position filter-form-visible filters) widget 
                                     (when (or (string= (getf args :id) "NIL") (and (not (getf filters :and)) (not (getf filters :or))))
                                       (setf filter-form-visible t)
                                       (setf filters nil))
                                     (setf filters 
                                           (car (map-filters 
                                                  (lambda (item)
                                                    (when (string= (string (getf item :id)) (getf args :id))
                                                      (let ((and-items (getf item :and)))
                                                        (setf item (first and-items))
                                                        (when (cdr and-items)
                                                          (setf (getf item :and) (cdr and-items)))))
                                                    item)
                                                  (list filters)))))
                                   (mark-dirty widget))))
    #+l(setf filters `(:value nil
                       :id ,(write-to-string (gensym))
                       :and nil
                       :or nil))
    (setf filters nil))

  (if (getf args :dataseq-instance) 
    (setf (dataseq-on-query (getf args :dataseq-instance)) (on-query-function widget)))

  (call-next-method))

(defmethod render-widget-body :around ((widget filtering-widget) &rest args)
  (with-slots (filters filter-form-visible add-filter-action) widget 
    (with-html 
      (cond 
        ((getf filters :value)
         (htm 
           (:div :style "overflow:auto;"
            (:div :style "padding:5px;float:left;"
             (render-filters widget filters)))))
        (filter-form-visible (render-filter-form widget)) 
        (t (render-link (lambda (&rest args) (setf filter-form-visible t)) "Add filter")))
      (:div :style "clear:both"))))

(defmethod render-filters ((widget filtering-widget) filters)
  (render-and-cells-new widget (list* filters (getf filters :and)) nil))

(defmethod render-filter-display-value ((widget filtering-widget) field compare-type compare-value)
  (with-html 
    (:b 
      (esc field))
    (esc (cond 
           ((string= compare-type "equal") " is equal to ")
           ((string= compare-type "like") " is like ")
           ((string= compare-type "not-like") " is not like ")
           ((string= compare-type "not-equal") " is not equal to ")
           (t (error (format nil "No such compare type - ~A" compare-type)))))
    (:b 
      (esc (get-filter-value-display-value widget field compare-type compare-value)))))

(defmethod get-filter-value-display-value ((widget filtering-widget) field compare-type compare-value)
  (format nil "~A" compare-value))

(defmethod render-filter-display ((widget filtering-widget) spec-plist)
  (with-html 
    (:div :style "white-space:nowrap;padding-right:10px;"
     (render-filter-display-value 
       widget
       (getf (find-form-field-by-id widget (getf spec-plist :field)) :caption)
       (getf spec-plist :compare-type) 
       (getf spec-plist :compare-value)))))

(defmethod find-form-field-by-id ((widget filtering-widget) keyword-id)
  (loop for i in (slot-value widget 'form-fields)
        if (equal (getf i :id) keyword-id)
        return i))

(defmethod render-bottom-add-filter-link-or-form ((widget filtering-widget) filter-id filter-type)
  (with-html 
    (if (filter-form-on-element-p widget filter-id filter-type)
      (htm 
        (:div :style #-DEBUG(append-css-border-radius "5px" "border:1px solid #dbeac1;padding:5px;") #+DEBUG"border:1px solid red;"
         (:div :style "float:left;"
          (render-filter-form widget))
         (:div :style "clear:both;")))
      (htm 
        (:div :style "text-align:center;"
         (render-add-filter-link widget filter-id filter-type))))))

(defmethod render-or-cells-new ((widget filtering-widget) or-cells parent &optional display-close-button-p)
  (let ((filter parent))
    (with-html 
      (:div :style #-DEBUG(append-css-border-radius "5px" "border:1px solid #dbeac1;height:100%;padding:5px;") #+DEBUG"border:1px solid yellow;height:100%;padding:5px;"
       (:div :style "float:right;" 
        (when (or (getf parent :or) (getf parent :and) display-close-button-p)
          (htm (:a 
                 :href (add-get-param-to-url (make-action-url (slot-value widget 'remove-filter-action)) "id" (getf filter :id))
                 :onclick (format nil 
                                  "initiateActionWithArgs(\"~A\", \"~A\", {id: \"~A\"});return false;"
                                  (slot-value widget 'remove-filter-action) (session-name-string-pair) (getf filter :id))
                 :href "" :style "text-decoration:none" "x"))))
       (:div (render-filter-display widget (getf parent :value)))
       (loop for filter in or-cells do 
             (htm 
               (unless (equal filter parent) 
                 (htm (:div :style "text-align:center" "or")))
               (:div
                 (unless (equal filter parent)
                   (render-and-cells-new 
                     widget 
                     (list* filter (getf filter :and))
                     filter))))) 
       (htm 
         (:div :style "text-align:center" "or")
         (:div :style "text-align:center"
          (render-bottom-add-filter-link-or-form widget (or (getf parent :id) "") :or)))))))

(defmethod render-and-cells-new ((widget filtering-widget) and-cells parent)
  (with-html
    (:table :cellpadding 5 :cellspacing 0 :style (append-css-border-radius "5px" "height:100%;margin:0 auto; border:1px solid #dbeac1;background-color:#ECF8D7")
     (:tr 
       (loop for filter in and-cells do
             (htm 
               (:td :valign "top"
                (render-or-cells-new widget (list* filter (reverse (getf filter :or))) filter (not (equal filter parent)))) 
               (:td "and")))
       (htm (:td
              (render-right-add-filter-link-or-form widget (getf (first and-cells) :id) :and))
            (:td :valign "top"
             (htm (:a 
                    :href (add-get-param-to-url (make-action-url (slot-value widget 'remove-filter-action)) "id" (getf parent :id ""))
                    :onclick (format nil 
                                     "initiateActionWithArgs(\"~A\", \"~A\", {id: \"~A\"});return false;"
                                     (slot-value widget 'remove-filter-action) (session-name-string-pair) (getf parent :id))
                    :href "" :style "text-decoration:none" "x"))))))))

(defmethod render-right-add-filter-link-or-form ((widget filtering-widget) id type)
  (if (filter-form-on-element-p widget id type)
    (with-html 
      (:div :style #-DEBUG(append-css-border-radius "5px" "border:1px solid #dbeac1;padding:5px;") #+DEBUG"border: 1px solid red;"
       (render-filter-form widget)))
    (render-add-filter-link widget id type)))

(defmethod render-add-filter-link ((widget filtering-widget) filter-id filter-type)
  (let* ((action (slot-value widget 'add-filter-action))
         (url (make-action-url action)))
    (with-html
      (:a 
        :href (add-get-param-to-url 
                (add-get-param-to-url url "id" filter-id)
                "filter-type"
                (string filter-type))
        :onclick (format nil 
                         "initiateActionWithArgs(\"~A\", \"~A\", {id: \"~A\", \"filter-type\":\"~A\"});return false;"
                         action (session-name-string-pair) filter-id filter-type)
        :style "white-space:nowrap;" "Add filter"))))

(defmethod render-filter-form ((widget filtering-widget))
  (with-slots (filter-form-visible) widget 
    (when filter-form-visible 
      (render-widget (get-filter-form widget)))))

(defmethod hide-filter-form ((widget filtering-widget))
  (setf (slot-value widget 'filter-form-visible) nil)
  (setf (slot-value widget 'filter-form-position) nil)
  (mark-dirty widget))

(defmethod get-filter-form ((widget filtering-widget))
  (make-filtering-form 
    widget
    (list :field (getf (first (slot-value widget 'form-fields)) :id) :compare-type "like")
    :on-success (lambda (form object)
                  (with-slots (filter-form-position filters) widget 
                    (let* ((new-filter-value (object->simple-plist object))
                           (new-filter (list :value  nil
                                             :id (write-to-string (gensym))
                                             :and nil 
                                             :or nil))
                           (key (if filter-form-position (intern (cdr filter-form-position) "KEYWORD") :and)))
                      (if (equal key :top-or)
                        (progn 
                          (setf (getf new-filter :or) (list filters))
                          (setf filters new-filter))
                        (progn

                          (setf (getf new-filter-value :field) (intern (getf new-filter-value :field) "KEYWORD")) 
                          (setf (getf new-filter :value) new-filter-value) 

                          (cond 
                            ((not (getf filters :value)) (setf filters new-filter))
                            (t
                             (setf filters 
                                   (car (map-filters 
                                          (lambda (item)
                                            (if (string= (string (getf item :id)) (car filter-form-position))
                                              (progn
                                                (push new-filter (getf item key))
                                                item)
                                              item))
                                          (list filters))))))))))
                  (hide-filter-form widget)
                  (mark-dirty widget))
    :on-cancel (lambda (form)
                 (hide-filter-form widget))))

(defmethod compare-field-form-choices ((widget filtering-widget))
  (loop for i in (slot-value widget 'form-fields) 
        collect (cons (getf i :caption) 
                      (getf i :id))))

(defmethod mark-dirty :around ((widget filtering-widget) &key propagate putp) 
  (if (slot-boundp widget 'dataseq-instance)
    (with-slots (dataseq-instance) widget
      (if dataseq-instance
        (mark-dirty dataseq-instance)))) 
  (call-next-method))

(defmethod form-fields-accessors-list ((widget filtering-widget))
  (with-slots (form-fields) widget
    (loop for i in form-fields append (list (getf i :id) (getf i :accessor)))))

(defmethod on-query-function ((widget filtering-widget))
  (lambda (obj order limit &key countp)
    (let ((values 
            (funcall 
              (if countp #'count-by #'find-by)
              (dataseq-data-class obj)
              (lambda (item)
                (compare (slot-value widget 'filters) item 
                         (form-fields-accessors-list widget)))
              :order-by order
              :range limit 
              :store (dataseq-class-store obj))))
      values)))
