(in-package :weblocks-filtering-widget-tests)

(defvar *lorem* "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
(defvar *lorem-2* "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defstore *weblocks-filtering-widget-store* :memory)

(defclass test-model ()
  ((id) 
   (title :accessor test-model-title :initarg :title) 
   (content :accessor test-model-content :initarg :content)))

; From php-functions-for-cl
(defun implode (glue-or-pieces &optional (pieces nil pieces-given-p))
  (unless pieces-given-p 
    (return-from implode (implode "" glue-or-pieces)))

  (format nil "~{~A~}"
          (cdr (loop for i in pieces append 
                     (list glue-or-pieces i)))))

(setf (fdefinition 'join) (fdefinition 'implode))

(defun get-lorem-title-content (lorem-title-words lorem-content-words)
  (let* ((lorem-words (append 
                        (list "")
                        (ppcre:split "(,? )" lorem-title-words :with-registers-p t)))
         (lorem-words-count (ceiling (/ (length lorem-words) 2)))
         (lorem-paragraph-words (append 
                                  (list "")
                                  (ppcre:split "(,? )" lorem-content-words :with-registers-p t)))
         (lorem-paragraph-words-count (ceiling (/ (length lorem-paragraph-words) 2)))
         (lorem-title "")
         (lorem-paragraph-title "")
         (paragraph-words-to-words (* 2 (ceiling (/ lorem-paragraph-words-count lorem-words-count))))
         (subseq-start 0)
         (subseq-end paragraph-words-to-words)
         (result-1)
         (result-2))

    (loop for i from 0 for (key value) on lorem-words :by 'cddr
          do 
          (setf lorem-title (format nil "~A~A~A" lorem-title key value))
          (setf lorem-paragraph-title (format nil "~A~A" lorem-paragraph-title (join (subseq lorem-paragraph-words subseq-start subseq-end))))
          (push (list lorem-title lorem-paragraph-title) result-1)
          (setf subseq-start subseq-end)
          (setf subseq-end (min (length lorem-paragraph-words)
                                (+ subseq-end paragraph-words-to-words))))

    (setf lorem-title "")
    (setf lorem-words (append (list "") (reverse (cdr lorem-words))))
    (setf lorem-paragraph-words (append  (subseq (cdr lorem-paragraph-words) 0 (1- (length lorem-paragraph-words))) (list "")))

    (loop for i from lorem-words-count downto 0 for (key value) on lorem-words :by 'cddr
          do 
          (setf subseq-start (* (1- i) paragraph-words-to-words))
          (setf subseq-end (min (length lorem-paragraph-words) (* i paragraph-words-to-words)))
          (setf lorem-title (format nil "~A~A~A" value key lorem-title))
          (setf lorem-paragraph-title (format nil "~A~A" (join (subseq lorem-paragraph-words subseq-start subseq-end)) lorem-paragraph-title ))
          (push (list lorem-title lorem-paragraph-title) result-2))
    (append 
      (reverse result-1)
      (subseq result-2 1))))

(defun maybe-add-demo-records-to-test-model ()
  (when (zerop (length (all-of 'test-model :store *weblocks-filtering-widget-store*)))
    (loop for (title content) in (get-lorem-title-content *lorem* *lorem-2*) do
          (persist-object 
            *weblocks-filtering-widget-store*
            (make-instance 
              'test-model
              :title title
              :content content)))))

(defun filtering-widget-demonstration-action (&rest args)
  (maybe-add-demo-records-to-test-model)
  (let* ((widget)
         (composite-widget (make-instance 'composite)))
    (setf widget (make-instance 'datagrid :data-class 'test-model 
                                :sort (cons 'id :asc)
                                :class-store *weblocks-filtering-widget-store*))
    (setf (composite-widgets composite-widget) 
          (list 
            (lambda (&rest args)
              (with-html 
                (:h1 "Filtering widget demonstration")))
            (make-instance 
              'weblocks-filtering-widget:filtering-widget 
              :dataseq-instance widget 
              :form-fields (list 
                             (list :id :title 
                                   :caption "Title"
                                   :accessor #'test-model-title)
                             (list :id :content 
                                   :caption "Content"
                                   :accessor #'test-model-content)))
            widget 
            (lambda (&rest args)
              (render-link (lambda (&rest args)
                             (answer composite-widget t)) "back"))))
    (do-page composite-widget)))

(define-demo-action "Filtering widget" #'filtering-widget-demonstration-action)
