;; Load Packages
(load "mat_mul.lisp")

(in-package :lispgrad/nn)


;; mat-mul tests
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (3 4 5)) '(3 3)))
(defparameter *t2* (make-tensor '((2 3 4) (4 5 6) (3 4 5)) '(3 3)))
(defparameter *t3* (mat-mul *t1* *t2*))

;; transpose tests
(defparameter *t4* (transpose *t1*))


(assert (equal (tensor-data *t3*) '((93 123 153) (93 123 153) (93 123 153))) ())
(assert (equal (tensor-data *t4*) '((1 3 3) (2 4 4) (3 5 5))) ())
(terpri )
(write "All Matrix Multiplication Tests Passed!")