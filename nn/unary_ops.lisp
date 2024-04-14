;; unary_ops.lisp

;; Load Packages
(load (merge-pathnames "../config.lisp" *load-truename*))
(in-package :lispgrad/utils)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))


;; description:
;;  (tensor-neg t1): Performs element-wise negation of a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A new tensor resulting from the element-wise negation.
(defun tensor-neg (t1)
  (make-tensor
    (tensor-op #'(lambda (x) (* x -1)) t1)
    (tensor-shape t1)))

;; description:
;;  (tensor-abs t1): Applies the absolute value function element-wise to a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A new tensor with the absolute value function applied element-wise.
(defun tensor-abs (t1)
  (make-tensor
    (tensor-op #'abs t1)
    (tensor-shape t1)))

;; description:
;;  (tensor-exp t1): Applies the exponential function element-wise to a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A new tensor with the exponential function applied element-wise.
(defun tensor-exp (t1)
  (make-tensor
    (tensor-op #'exp t1)
    (tensor-shape t1)))

;; description:
;;  (tensor-log t1): Applies the natural logarithm element-wise to a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A new tensor with the natural logarithm applied element-wise.
(defun tensor-log (t1)
  (make-tensor
    (tensor-op #'log t1)
    (tensor-shape t1)))
