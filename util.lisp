(in-package :weblocks-filtering-widget)

(defun prevalence-poweredp (&key store)
  (let ((store (or store *default-store*)))
    (if (find-package 'cl-prevalence)
      (eq (class-name  (class-of store)) (intern "GUARDED-PREVALENCE-SYSTEM" "CL-PREVALENCE"))
      nil)))

(defun clsql-poveredp (&key store)
  (let ((store (or store *default-store*)))
    (and 
      (find-package 'clsql)
      (find-package 'clsql-fluid-bt)
      (string= (string (class-name (class-of store))) (string (intern "FLUID-DATABASE" "CLSQL-FLUID-BT"))))))

(defun find-by-in-sql-store (class fun &key order-by range store)
  (if (not range)
    (remove-if-not 
      fun 
      (find-persistent-objects 
        store class 
        :order-by order-by 
        :range range))
    (let* ((result)
           (needed-items-count (funcall (find-symbol "RANGE-TO-LIMIT" "WEBLOCKS-CLSQL") range))
           (new-range (cons 0 needed-items-count))
           (items-found 0))
      (loop do 
            (let ((items 
                    (find-persistent-objects 
                      store class 
                      :order-by order-by 
                      :range new-range)))
              (loop for item in items 
                    if 
                    (and 
                      (or (< (length result) needed-items-count) (return-from find-by-in-sql-store result))
                      (funcall fun item))
                    do 
                    (if (< items-found (car range))
                      (incf items-found)
                      (push item result)))
              (format t "Range ~A ~A ~A~%" new-range range items-found)
              (incf (car new-range) needed-items-count)
              (incf (cdr new-range) needed-items-count)))
      result)))

; XXX Not tested
(defun count-by-in-sql-store (class fun &key order-by range store)
  (if (not range)
    (remove-if-not 
      fun 
      (find-persistent-objects 
        store class 
        :order-by order-by 
        :range range))
    (let* ((result-count 0)
           (needed-items-count (funcall (find-symbol "RANGE-TO-LIMIT" "WEBLOCKS-CLSQL") range))
           (new-range (cons 0 needed-items-count))
           (items-found 0))
      (loop do 
            (let ((items 
                    (find-persistent-objects 
                      store class 
                      :order-by order-by 
                      :range new-range)))
              (loop for item in items 
                    if (funcall fun item)
                    do 
                    (if (< items-found (car range))
                      (incf items-found)
                      (incf result-count)))
              (format t "Range ~A ~A ~A~%" new-range range items-found)
              (incf (car new-range) needed-items-count)
              (incf (cdr new-range) needed-items-count)))
      result)))

(defun find-by (class fun &key order-by range store)
  (declare (special *default-store*))
  (let ((store (or store *default-store*)))
    (cond 
      ((prevalence-poweredp :store store)
       (find-persistent-objects store class 
                                :filter fun 
                                :order-by order-by 
                                :range range))
      ((clsql-poveredp :store store)
       (if fun 
         (find-by-in-sql-store 
           class fun 
           :order-by order-by 
           :range range 
           :store store)
         (find-persistent-objects 
           store class 
           :order-by order-by 
           :range range)))
      (t (find-persistent-objects store class 
                                  :filter-fn 
                                  (lambda (item) (not (funcall fun item))) 
                                  :order-by order-by 
                                  :range range)))))

; TODO count-by-in-sql-store not used yet
(defun count-by (class fun &key store &allow-other-keys)
  (let ((store (or store *default-store*)))
    (if (prevalence-poweredp :store store)
      (count-persistent-objects store class 
                               :filter fun)
      (count-persistent-objects store class 
                               :filter-fn 
                               (lambda (item) (not (funcall fun item)))))))

