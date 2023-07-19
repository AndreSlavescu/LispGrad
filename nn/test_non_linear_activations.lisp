;; Load Packages
(load "non_linear_activations.lisp")

(in-package :lispgrad/nn)


;; relu tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (relu *t1*))
(assert (equal (tensor-data *t2*) '(0 0 1 2)) ())
(terpri )
(write "All ReLU Tests Passed!")


;; relu-6 tests
(defparameter *t1* (make-tensor '(-1 0 1 7) '(4)))
(defparameter *t2* (relu-6 *t1*))
(assert (equal (tensor-data *t2*) '(0 0 1 6)) ())
(terpri )
(write "All ReLU-6 Tests Passed!")


;; celu tests
(defparameter *t1* (make-tensor '(-1 0 1 7) '(4)))
(defparameter *t2* (celu *t1* 1))
(assert (equal (tensor-data *t2*) '(-0.63212055 0 1 7)) ())
(terpri )
(write "All CELU Tests Passed!")


;; clip tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (clip *t1* 0 3))
(assert (equal (tensor-data *t2*) '(0 0 1 2)) ())
(terpri )
(write "All Clip Tests Passed!")


;; hardtanh tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (hard-tanh *t1* 0 3))
(assert (equal (tensor-data *t2*) '(0 0 1 2)) ())
(terpri )
(write "All HardTanh Tests Passed!")