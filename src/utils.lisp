(uiop:define-package :src/utils
    (:use :common-lisp)
  (:export
   #:copy-instance))

(in-package :src/utils)

(defun copy-instance (object &rest initargs)
  (let ((copy (allocate-instance (class-of object))))
    (dolist (slot (sb-mop:class-slots (class-of object)))
      (let ((slot-name (sb-mop:slot-definition-name slot)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name)))))
    (apply #'reinitialize-instance copy initargs)))
