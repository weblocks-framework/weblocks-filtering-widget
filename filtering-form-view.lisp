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
            (string= compare-type "not-equal"))
        (setf compare-type "like"))))
  (second (slot-value view 'weblocks::fields)))

(defmethod get-value-for-field-and-comparator ((view filtering-form-view) field comparator)
  (third (slot-value view 'weblocks::fields)))

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
