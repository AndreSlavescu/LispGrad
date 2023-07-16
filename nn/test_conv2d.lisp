;; Load Packages
(load "conv2d.lisp")

(in-package :lispgrad/nn)


;; conv2d tests
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (6 7 8)) '(3 3)))
(defparameter *k1* (make-tensor '((2 2) (2 2)) '(2 2)))
(defparameter *t3* (conv-2d *t1* *k1*))
(assert (equal (tensor-data *t3*) '((48 88) (48 88))) ())
(terpri )
(write "All Conv2d Tests Passed!")