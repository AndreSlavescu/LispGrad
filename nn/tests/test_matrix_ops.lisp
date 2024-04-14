;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))
(use-package 'nn_package)
(load (merge-pathnames "matrix_ops.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "utils_package.lisp" *utils-dir*))
(use-package 'utils_package)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "Starting Matrix Operation Tests...")

;; input tensors
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (3 4 5)) '(3 3)))
(defparameter *t2* (make-tensor '((2 3 4) (4 5 6) (3 4 5)) '(3 3)))

;; mat-mul tests
(defparameter *t3* (mat-mul *t1* *t2*))
(assert-close (tensor-data *t3*) '((93 123 153) (93 123 153) (93 123 153)) 0.0001)

;; transpose tests
(defparameter *t4* (transpose *t1*))
(assert-close (tensor-data *t4*) '((1 3 3) (2 4 4) (3 5 5)) 0.0001)

(write "All Matrix Operation Tests Passed!")
(terpri )
(terpri )
