;; Load Packages
(load "nn_package.lisp")

(in-package :lispgrad/nn)


;; description:
;;  (transpose (t1)): Transposes the tensor
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor that is the transpose of t1
;; example:
;;  (transpose '((1 2) (3 4))) ; returns '((1 3) (2 4))
(defun transpose (t1)
  (make-tensor 
    (apply #'mapcar #'list (tensor-data t1)) 
    (tensor-shape t1)))

;; description:
;;  (mat-mul (t1 t2)): Performs matrix multiplication of two tensors.
;; requires:
;;  t1: valid tensor
;;  t2: valid tensor
;;  both tensors are square matrices with same dimension
;; returns:
;;  A tensor that is the computed matrix multiplication of t1 and t2
;; example:
;;  (mat-mul '((1 2) (3 4)) '((1 2) (1 2))) ; returns '((3 6) (7 14))
(defun mat-mul (t1 t2)
  (let* ((data1 (tensor-data t1))
         (data2 (tensor-data (transpose t2))) 
         (shape1 (tensor-shape t1))
         (shape2 (tensor-shape t2))
         (n (first shape1)) 
         (m (first shape2)) 
         (common-dim (second shape1)))
    (assert (equal (first shape1) (second shape1)) ())
    (assert (equal (first shape2) (second shape2)) ())
    (assert (equal shape1 shape2) ())
    (assert (eq common-dim (second shape2)))
    (let ((result (make-list n :initial-element (make-list m :initial-element 0))))
      (dotimes (i n)
        (dotimes (j m)
          (dotimes (k common-dim)
            (setf (nth j (nth i result)) (+ (nth j (nth i result)) (* (nth k (nth i data1)) (nth k (nth j data2))))))))
      (make-tensor result (list n m)))))
