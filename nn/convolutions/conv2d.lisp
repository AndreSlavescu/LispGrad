;; Load Packages
(load "nn_package.lisp")

(in-package :lispgrad/nn)


;; description:
;;  (conv-2d (t1 kernel)): Computes 2D convolution of an input tensor with a 2D kernel.
;; requires:
;;  t1: valid 2D tensor
;;  kernel: valid 2D tensor
;;  kernel shape <= t1 shape
;; returns:
;;  A tensor that is the computed 2D convolution of the input with applying the kernel.
;; example:
;;  (conv-2d '((1 1 1) (0 1 0) (0 0 1)) '((1 0) (0 1))) ; returns '((3 2) (3 2))
(defun conv-2d (t1 kernel)
  (let* ((t1-data (tensor-data t1))
         (kernel-data (tensor-data kernel))
         (t1-shape (tensor-shape t1))
         (kernel-shape (tensor-shape kernel))
         (n (- (first t1-shape) (first kernel-shape) -1))
         (m (- (second t1-shape) (second kernel-shape) -1)))

    (let ((res (make-list n :initial-element (make-list m :initial-element 0))))
      (dotimes (i n)
        (dotimes (j m)
          (dotimes (k (first kernel-shape))
            (dotimes (l (second kernel-shape))
              (incf (nth j (nth i res)) 
                    (* (nth (+ i k) (nth (+ j l) t1-data)) 
                       (nth k (nth l kernel-data))))))))
      (make-tensor res (list n m)))))
