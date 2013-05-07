(in-package :weblocks-filtering-widget-tests)

(def-test-suite weblocks-filtering-widget-tests)

(deftest filters-records ()
  (with-new-or-existing-selenium-session-on-jquery-site
    (do-click-and-wait "link=Filtering widget")
    (do-type "name=compare-value" "Lorem")
    (do-click-and-wait "name=submit")
    (is (string= (do-get-text "css=td.title:first") "Lorem"))
    (is (string= (do-get-text "css=td.title:last") "Lorem ipsum dolor sit amet, consectetur adipisicing elit"))))
