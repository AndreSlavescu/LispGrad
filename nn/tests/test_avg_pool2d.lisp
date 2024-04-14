;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "pooling/avg_pool2d.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "\n\nStarting AvPool2d Tests...")

;; avgpool2d tests
(defparameter *t1* (make-tensor '((1 2 3) (3 4 5) (6 7 8)) '(3 3)))
(defparameter *k1* (make-tensor '((1 1) (1 1)) '(2 2)))
(defparameter *t3* (avg-pool-2d *t1* *k1* 1))
(assert-close (tensor-data *t3*) '((3/2 11/4) (3/2 11/4)) 0.0001)
(terpri )


(write "All AvgPool2d Tests Passed!")
(terpri )
(terpri )
