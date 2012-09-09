;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-filtering-widget-asd
  (:use :cl :asdf))

(in-package :weblocks-filtering-widget-asd)

(defsystem weblocks-filtering-widget
    :name "Weblocks filtering widget"
    :version "0.1"
    :author "Olexiy Zamkoviy"
    :licence "Public Domain"
    :description "Filtering widget for weblocks framework"
    :depends-on (:weblocks)
    :components ((:file "package")
                 (:file "filtering-widget" :depends-on ("package"))
                 (:file "filtering-form" :depends-on ("package"))))

