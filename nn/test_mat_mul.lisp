(load "mat_mul.lisp")

(in-package :lispgrad/nn)

;; (defun test-mat-mul ()
;;   (let* ((data1 '((1 2 3 4 5) (3 4 5 6 7)))
;;          (data2 '((2 3 4 5 6) (4 5 6 7 8)))
;;          (shape1 (list-shape data1))
;;          (shape2 (list-shape data2))
;;          (tensor1 (make-tensor data1 shape1))
;;          (tensor2 (make-tensor data2 shape2)))
;;     (let ((result (mat-mul tensor1 tensor2)))
;;       (print (tensor-data result)) 
;;       (print (tensor-shape result)))))

;; (test-mat-mul)
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (3 4 5)) '(3 3)))
(defparameter *t2* (make-tensor '((2 3 4) (4 5 6) (3 4 5)) '(3 3)))
(defparameter *t3* (mat-mul *t1* *t2*))

(assert (equal (tensor-data *t3*) '((93 123 153) (93 123 153) (93 123 153))) ())
(terpri )
(write "All Matrix Multiplication Tests Passed!")