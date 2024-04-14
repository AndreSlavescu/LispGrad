;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))
(use-package 'nn_package)
(load (merge-pathnames "binary_ops.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "utils_package.lisp" *utils-dir*))
(use-package 'utils_package)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "Starting Binary Operator Tests...")

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

(assert-close (tensor-data *t3*) '((3 5) (7 9)) 0.0001)
(assert-close (tensor-data *t4*) '((-1 -1) (-1 -1)) 0.0001)
(assert-close (tensor-data *t5*) '((2 6) (12 20)) 0.0001)
(assert-close (tensor-data *t6*) '((1/2 2/3) (3/4 4/5)) 0.0001)
(assert-close (tensor-data *t7*) '((0 2) (0 4)) 0.0001)
(assert-close (tensor-data *t8*) '((3 3) (7 5)) 0.0001)
(assert-close (tensor-data *t9*) '((3 1) (7 1)) 0.0001)
(terpri )

(write "All Binary Operator Tests Passed!")
(terpri )
(terpri )
