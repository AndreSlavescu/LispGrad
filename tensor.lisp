;; tensor package
(defpackage :lispgrad
  (:nicknames :lispgrad)
  (:use :common-lisp)
  (:export 
    #:Tensor 
    #:tensor-data
    #:tensor-shape
    #:list-shape 
    #:make-tensor 
    #:tensor-op))

(in-package :lispgrad)


;; description:
;;  Tensor class for LispGrad.
;; args:
;;  data: List of size shape
;;  shape: List
(defclass Tensor ()
  ((data
    :initarg :data
    :accessor tensor-data)
   (shape
    :initarg :shape
    :accessor tensor-shape)))

;; description: 
;;  (compute-shape data): Compute the shape of the provided data.
;; requires: 
;;  data: List
;; returns: 
;;  A list representing the shape of the input data.
;; example: 
;;  (compute-shape '((1 2) (3 4))) ; returns '(2 2)
(defun list-shape (lst)
  (if (not (listp lst))
      '()
      (cons (length lst) (list-shape (first lst)))))

;; description: 
;;  (make-tensor (data shape)): Create a tensor with specified data and shape.
;; requires: 
;;  data: List
;;  shape: List
;; returns:
;;  A Tensor Object.
;; example: 
;;  (make-tensor '((1 2) (3 4)) '(2 2)))
(defun make-tensor (data shape)
  (assert (equal shape (list-shape data)) ())
  (make-instance 'tensor :data data :shape shape))

;; description:
;;  tensor-op (op t1 &optional t2): Apply arbitrary operator on one or two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: optional, valid tensor
;;  t1 shape is equal to t2 shape for binary operations
;; returns:
;;  Result of the operation applied on t1 and optionally t2
;; example:
;;  (tensor-op #'+ '(1 2) '(2 3)) ; returns '(3 5)
;;  (tensor-op #'- '(1 2) '(2 3)) ; returns '(-1 -1)
;;  (tensor-op #'abs '(-1 -2)) ; returns '(1 2)
(defun tensor-op (op t1 &optional t2)
  "Applies an operation element-wise to one or two tensors.
   Requires:
   - op: The operation to apply (e.g., #'+, #'-, #'*, #'/ for binary, #'abs, #'exp for unary)
   - t1: A tensor (list or array)
   - t2: An optional second tensor of the same shape as t1 for binary operations
   Returns: A new tensor resulting from applying the operation."
  (if t2
      (progn
        ;; Binary Operation
        (assert (equal (tensor-shape t1) (tensor-shape t2)) ())
        (let ((data1 (tensor-data t1))
              (data2 (tensor-data t2)))
          (labels ((rec (l1 l2)
                    (if (listp (car l1))
                        (mapcar #'rec l1 l2)
                        (mapcar op l1 l2))))
            (rec data1 data2))))
      ;; Unary operation
      (let ((data (tensor-data t1)))
        (labels ((rec (l)
                    (if (listp (car l))
                        (mapcar #'rec l)
                        (mapcar op l))))
          (rec data)))))
