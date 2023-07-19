;; Load Packages
(load "avg_pool2d.lisp")

(in-package :lispgrad/nn)


;; avgpool2d tests
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (6 7 8)) '(3 3)))
(defparameter *k1* (make-tensor '((1 1) (1 1)) '(2 2)))
(defparameter *t3* (avg-pool-2d *t1* *k1* 1))
(assert (equal (tensor-data *t3*) '((3/2 11/4) (3/2 11/4))) ())
(terpri )
(write "All AvgPool2d Tests Passed!")
