;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-filtering-widget-asd
  (:use :cl :asdf))

(in-package :weblocks-filtering-widget-asd)

(let ((dependencies (list :weblocks :cl-config :anaphora :cl-containers :weblocks-utils)))
   (if (find-package :clsql) 
    (push :clsql dependencies))
   (eval `(defsystem weblocks-filtering-widget
           :name "Weblocks filtering widget"
           :version "0.3.3"
           :author "Olexiy Zamkoviy"
           :licence "LLGPL"
           :description "Filtering widget for weblocks framework"
           :depends-on ,dependencies
           :components ((:file "package")
             (:file "filtering-widget" :depends-on ("package" "compare-function" "links-choices-presentation"))
             (:file "filtering-form" :depends-on ("package"))
             (:file "compare-function" :depends-on ("package" "compare-sql-function"))
             (:file "compare-sql-function" :depends-on ("package"))
             (:file "links-choices-presentation" :depends-on ("package"))
             (:file "filtering-form-view" :depends-on ("package"))))))

