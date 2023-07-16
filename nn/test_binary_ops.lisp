;; Load Packages
(load "binary_ops.lisp")

(in-package :lispgrad/nn)


;; tests
(defparameter *t1* (make-tensor '((1 2) (3 4)) '(2 2)))
(defparameter *t2* (make-tensor '((2 3) (4 5)) '(2 2)))
(defparameter *t3* (tensor-add *t1* *t2*))
(defparameter *t4* (tensor-sub *t1* *t2*))
(defparameter *t5* (tensor-mul *t1* *t2*))
(defparameter *t6* (tensor-div *t1* *t2*))
(defparameter *t7* (tensor-land *t1* *t2*))
(defparameter *t8* (tensor-lior *t1* *t2*))
(defparameter *t9* (tensor-lxor *t1* *t2*))

(assert (equal (tensor-data *t3*) '((3 5) (7 9))) ())
(assert (equal (tensor-data *t4*) '((-1 -1) (-1 -1))) ())
(assert (equal (tensor-data *t5*) '((2 6) (12 20))) ())
(assert (equal (tensor-data *t6*) '((1/2 2/3) (3/4 4/5))) ())
(assert (equal (tensor-data *t7*) '((0 2) (0 4))) ())
(assert (equal (tensor-data *t8*) '((3 3) (7 5))) ())
(assert (equal (tensor-data *t9*) '((3 1) (7 1))) ())
(terpri )
(write "All Binary Operator Tests Passed!")