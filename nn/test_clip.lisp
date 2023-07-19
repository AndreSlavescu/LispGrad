;; Load Packages
(load "clip.lisp")

(in-package :lispgrad/nn)


;; clip tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (clip *t1* 0 3))
(assert (equal (tensor-data *t2*) '(0 0 1 2)) ())
(terpri )
(write "All Clip Tests Passed!")

