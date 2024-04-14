;; Load Packages
(load (merge-pathnames "../../config.lisp" *load-truename*))
(in-package :lispgrad/nn)
(load (merge-pathnames "non_linear_activations.lisp" *nn-dir*))

(in-package :lispgrad/utils)
(load (merge-pathnames "assertion_tools.lisp" *utils-dir*))


(terpri )
(terpri )
(write "Starting Activation Function Tests...")

;; relu tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (relu *t1*))
(assert-close (tensor-data *t2*) '(0 0 1 2) 0.0001) ()
(terpri )
(write "All ReLU Tests Passed!")


;; relu-6 tests
(defparameter *t1* (make-tensor '(-1 0 1 7) '(4)))
(defparameter *t2* (relu6 *t1*))
(assert-close (tensor-data *t2*) '(0 0 1 6) 0.0001) ()
(terpri )
(write "All ReLU-6 Tests Passed!")


;; celu tests
(defparameter *t1* (make-tensor '(-1 0 1 7) '(4)))
(defparameter *t2* (celu *t1* 1))
(assert-close (tensor-data *t2*) '(-0.63212055 0 1 7) 0.0001) ()
(terpri )
(write "All CELU Tests Passed!")


;; clip tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (clip *t1* 0 3))
(assert-close (tensor-data *t2*) '(0 0 1 2) 0.0001) ()
(terpri )
(write "All Clip Tests Passed!")


;; hardtanh tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (hard-tanh *t1* 0 3))
(assert-close (tensor-data *t2*) '(0 0 1 2) 0.0001) ()
(terpri )
(write "All HardTanh Tests Passed!")


;; sigmoid tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (sigmoid *t1*))
(assert-close (tensor-data *t2*) '(0.26894142 0.5 0.73105858 0.880797) 0.0001) ()
(terpri )
(write "All Sigmoid Tests Passed!")


;; tanh tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (tanh-activation *t1*))
(assert-close (tensor-data *t2*) '(-0.7615942 0.0 0.7615942 0.9640276) 0.0001) ()
(terpri )
(write "All Tanh Tests Passed!")


;; gelu tests
(defparameter *t1* (make-tensor '(-1 0 1 2) '(4)))
(defparameter *t2* (gelu *t1*))
(defun round-to (number decimals)
  (/ (round (* number (expt 10 decimals)))
     (expt 10 decimals)))
(assert-close (mapcar #'(lambda (x) (round-to x 6)) (tensor-data *t2*))
              (mapcar #'(lambda (x) (round-to x 6)) '(-0.1586553 0.0 0.8413448 1.9544997)) 0.0001) ()
(terpri )
(write "All GELU Tests Passed!")

(write "All Activation Function Tests Passed!")
(terpri )
(terpri )
