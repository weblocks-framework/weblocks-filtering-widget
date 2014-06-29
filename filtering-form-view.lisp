(in-package :weblocks-filtering-widget)

(defclass filtering-form-view (form-view)
  ((form :initform nil)))

(defmethod get-comparator-for-field ((view filtering-form-view) field)
  (with-slots (form) view 
    (with-slots (compare-type) (dataform-data form)
      (unless 
        (or (string= compare-type "like")
            (string= compare-type "equal")
            (string= compare-type "not-like")
            (string= compare-type "not-equal")
            (string= compare-type "greater-number")
            (string= compare-type "less-number")
            (string= compare-type "greater-date")
            (string= compare-type "less-date")
            (string= compare-type "identical")
            (string= compare-type "not-identical")
            (string= compare-type "null")
            (string= compare-type "not-null"))
        (setf compare-type "like"))))
  (second (slot-value view 'weblocks::fields)))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field comparator)
  (third (slot-value view 'weblocks::fields)))

(defmethod get-value-for-field-and-comparator :around ((view filtering-form-view) field comparator)
  (if (find comparator (list :greater-date :less-date))
    (progn 
      (with-slots (form) view 
        (with-slots (compare-value) (dataform-data form)
          (unless 
            (integerp compare-value)
            (setf compare-value nil)))) 
      (make-instance 
        'form-view-field
        :slot-name 'compare-value 
        :present-as (make-instance 'weblocks-bootstrap-date-entry-presentation:bootstrap-date-entry-presentation)
        :parse-as (make-instance 'weblocks-bootstrap-date-entry-presentation:bootstrap-date-parser)))
    (call-next-method)))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field (comparator (eql :identical)))
  (with-slots (form) view 
    (with-slots (compare-value) (dataform-data form)
      (unless 
        (stringp compare-value)
        (setf compare-value nil))))

  (let* ((fields (slot-value 
                  (slot-value 
                    (slot-value view 'weblocks::form)
                    'filtering-widget-instance)
                  'form-fields))
        (field-meta (get-field-data-by-id fields field)))

    (make-instance 
      'form-view-field
      :slot-name 'compare-value 
      :present-as 
      (make-instance 
        'dropdown-presentation 
        :choices (or 
                   (getf field-meta :choices-callback)
                   '(("Please specify :choices-callback attribute for filter". nil)))))))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field (comparator (eql :not-identical)))
  (get-value-for-field-and-comparator view field :identical))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field (comparator (eql :null)))
  (make-instance 
    'form-view-field
    :hidep t
    :slot-name 'compare-value 
    :present-as (make-instance 'html-presentation) 
    :reader (lambda (&rest args)
              "")))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field (comparator (eql :not-null)))
  (get-value-for-field-and-comparator view field :null))

(defmethod view-fields :around ((view filtering-form-view))
  (let ((fields (call-next-method))
        (filter-field 
          (intern 
            (string-upcase 
              (slot-value 
                (dataform-data (slot-value view 'form))
                'field))
            "KEYWORD")))
    (list 
      (car fields)
      (get-comparator-for-field 
        view 
        filter-field)
      (get-value-for-field-and-comparator 
        view 
        filter-field 
        (intern 
          (string-upcase 
            (slot-value 
              (dataform-data (slot-value view 'form))
              'compare-type))
          "KEYWORD")))))

(defclass filtering-form-view-field (form-view-field)
  ())
