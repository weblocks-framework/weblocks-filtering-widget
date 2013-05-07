(defpackage #:weblocks-filtering-widget
  (:use :cl :weblocks :cl-who :anaphora :cl-containers)
  (:import-from :weblocks-utils :find-by :count-by :clsql-poweredp :all-of)
  (:documentation
    "Filtering widget for weblocks framework")
  (:export #:filtering-widget #:filtering-form #:on-query-function))

