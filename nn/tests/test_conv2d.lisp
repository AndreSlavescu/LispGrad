;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))
(use-package 'nn_package)
(load (merge-pathnames "convolutions/conv2d.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "utils_package.lisp" *utils-dir*))
(use-package 'utils_package)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "Starting Conv2d Tests...")

;; conv2d tests
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (6 7 8)) '(3 3)))
(defparameter *k1* (make-tensor '((2 2) (2 2)) '(2 2)))
(defparameter *t3* (conv-2d *t1* *k1*))
(assert-close (tensor-data *t3*) '((48 88) (48 88)) 0.0001)
(terpri )

(write "All Conv2d Tests Passed!")
(terpri )
(terpri )
