(defpackage #:weblocks-filtering-widget
  (:use :cl :weblocks :cl-who :anaphora :cl-containers :weblocks-util :weblocks-stores)
  (:import-from :weblocks-utils :find-by :count-by :clsql-poweredp :all-of)
  (:documentation
    "Filtering widget for weblocks framework")
  (:export #:filtering-widget 
           #:filtering-form 
           #:on-query-function 
           #:get-comparator-for-field 
           #:get-value-for-field-and-comparator 
           #:get-filter-value-display-value 
           #:filtering-form-view 
           #:*compare-functions* 
           #:compare-type 
           #:compare-value 
           #:links-choices-presentation 
           #:all-filters-for-model 
           #:force-string))

