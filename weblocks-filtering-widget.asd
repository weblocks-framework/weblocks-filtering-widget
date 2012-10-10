;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-filtering-widget-asd
  (:use :cl :asdf))

(in-package :weblocks-filtering-widget-asd)

(let ((dependencies (list :weblocks :cl-config)))
   (if (find-package :clsql) 
    (push :clsql dependencies))
   (eval `(defsystem weblocks-filtering-widget
           :name "Weblocks filtering widget"
           :version "0.1"
           :author "Olexiy Zamkoviy"
           :licence "Public Domain"
           :description "Filtering widget for weblocks framework"
           :depends-on ,dependencies
           :components ((:file "package")
             (:file "filtering-widget" :depends-on ("package" "compare-function"))
             (:file "filtering-form" :depends-on ("package"))
             (:file "compare-function" :depends-on ("package" "util" "compare-sql-function"))
             (:file "util" :depends-on ("package"))
             (:file "compare-sql-function" :depends-on ("package"))))))

