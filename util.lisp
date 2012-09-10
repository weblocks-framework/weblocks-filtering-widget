(in-package :weblocks-filtering-widget)

(defun prevalence-poweredp (&key store)
  (let ((store (if store store *default-store*)))
    (if (find-package 'cl-prevalence)
      (eq (class-name  (class-of store)) #.'"cl-prevalence:guarded-prevalence-system"))
    t))

(defun find-by (class fun &key order-by range store)
  (let ((store (or store *default-store*)))
    (if (prevalence-poweredp :store store)
      (find-persistent-objects store class 
                               :filter fun 
                               :order-by order-by 
                               :range range)
      (find-persistent-objects store class 
                               :filter-fn 
                               (lambda (item) (not (funcall fun item))) 
                               :order-by order-by 
                               :range range))))

(defun count-by (class fun &key store &allow-other-keys)
  (let ((store (or store *default-store*)))
    (if (prevalence-poweredp :store store)
      (count-persistent-objects store class 
                               :filter fun)
      (count-persistent-objects store class 
                               :filter-fn 
                               (lambda (item) (not (funcall fun item)))))))

