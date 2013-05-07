;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-filtering-widget-tests-asd
  (:use :cl :asdf))

(in-package :weblocks-filtering-widget-tests-asd)

(defsystem weblocks-filtering-widget-tests
   :name "Weblocks filtering widget tests"
   :version "0.0.1"
   :author "Olexiy Zamkoviy"
   :licence "LLGPL"
   :description "Tests for weblocks filtering widget"
   :depends-on (:weblocks-selenium-tests :weblocks-filtering-widget :weblocks-utils :cl-ppcre)
   :components 
   ((:module "tests"
     :components
     ((:file "package")
      (:file "tests-app-updates" :depends-on ("package"))
      (:file "tests" :depends-on ("tests-app-updates"))))))

