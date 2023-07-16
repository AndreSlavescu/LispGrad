(load "../tensor.lisp")

(defpackage :lispgrad/nn
  (:nicknames :lispgrad.nn)
  (:use :common-lisp :lispgrad)
  (:export 
    ;; binary ops
    #:tensor-add 
    #:tensor-sub 
    #:tensor-mul 
    #:tensor-div
    #:tensor-land))

(in-package :lispgrad/nn)

;; description:
;;  (tensor-add (t1 t2)): Computes the sum of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  t1 shape is equal to t2 shape
;; returns:
;;  A tensor that is the computed sum of t1 and t2
;; example:
;;  (tensor-add '(1 2) '(2 3)) ; returns '(3 5)
(defun tensor-add (t1 t2)
  (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
  (make-tensor (tensor-op #'+ t1 t2) (tensor-shape t1)))


;; description:
;;  (tensor-sub (t1 t2)): Computes the difference of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  t1 shape is equal to t2 shape
;; returns:
;;  A tensor that is the computed difference of t1 and t2
;; example:
;;  (tensor-sub '(1 2) '(2 3)) ; returns '(-1 -1)
(defun tensor-sub (t1 t2)
  (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
  (make-tensor (tensor-op #'- t1 t2) (tensor-shape t1)))


;; description:
;;  (tensor-mul (t1 t2)): Computes the product of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  t1 shape is equal to t2 shape
;; returns:
;;  A tensor that is the computed product of t1 and t2
;; example:
;;  (tensor-add '(1 2) '(2 3)) ; returns '(2 6)
(defun tensor-mul (t1 t2)
  (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
  (make-tensor (tensor-op #'* t1 t2) (tensor-shape t1)))


;; description:
;;  (tensor-div (t1 t2)): Computes the quotient of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  t1 shape is equal to t2 shape
;; returns:
;;  A tensor that is the computed quotient of t1 and t2
;; example:
;;  (tensor-add '(1 2) '(2 3)) ; returns '(1/2 2/3)
(defun tensor-div (t1 t2)
  (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
  (make-tensor (tensor-op #'/ t1 t2) (tensor-shape t1)))


;; description:
;;  (tensor-land (t1 t2)): Computes the logical AND of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  t1 shape is equal to t2 shape
;; returns:
;;  A tensor that is the computed logical AND of t1 and t2
;; example:
;;  (tensor-land '(1 2) '(2 3)) ; returns '(0 2)
(defun tensor-land (t1 t2)
  (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
  (make-tensor (tensor-op #'logand t1 t2) (tensor-shape t1)))