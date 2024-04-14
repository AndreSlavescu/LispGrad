;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "unary_ops.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "Starting Unary Operator Tests...")

;; inout tensors
(defparameter *t1* (make-tensor '(-1 2 -3 4) '(4)))

;; tensor-neg
(defparameter *neg-t1* (tensor-neg *t1*))
(assert-close (tensor-data *neg-t1*) '(1 -2 3 -4) 0.0001)

;; tensor-abs
(defparameter *abs-t1* (tensor-abs *t1*))
(assert-close (tensor-data *abs-t1*) '(1 2 3 4) 0.0001)

;; tensor-exp
(defparameter *exp-t1* (tensor-exp *t1*))
(assert-close (tensor-data *exp-t1*) '(0.367879 7.389056 0.049787 54.598150) 0.0001)

;; tensor-log
(defparameter *log-t1* (tensor-log (make-tensor '(1 2 3 4) '(4))))
(assert-close (tensor-data *log-t1*) '(0.0 0.693147 1.098612 1.386294) 0.0001)

(write "All Unary Operator Tests Passed!")
(terpri )
(terpri )
