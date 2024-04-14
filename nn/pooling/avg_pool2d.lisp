;; Load Packages
(load "nn_package.lisp")

(in-package :lispgrad/nn)

(defun flatten (lst)
  (if (null lst)
      nil
      (if (listp (first lst))
          (append (flatten (first lst)) (flatten (rest lst)))
          (cons (first lst) (flatten (rest lst))))))


;; description:
;;  (avg-pool-2d (t1 kernel)): Computes 2D average pool of an input tensor with a 2D kernel.
;; requires:
;;  t1: valid 2D tensor
;;  kernel: valid 2D tensor
;;  kernel shape <= t1 shape
;; returns:
;;  A tensor that is the computed 2D average pool of the input with applying the kernel.
;; example:
;;  (avg-pool-2d '((1 1 1) (0 1 0) (0 0 1)) '((1 0) (0 1))) ; returns '((1/2 1/4) (0 1/2))
(defun avg-pool-2d (t1 kernel stride)
  (let* ((t1-data (tensor-data t1))
         (kernel-shape (tensor-shape kernel))
         (t1-shape (tensor-shape t1))
         (kernel-size (first kernel-shape)) 
         (n (+ 1 (floor (- (first t1-shape) kernel-size) stride)))
         (m (+ 1 (floor (- (second t1-shape) kernel-size) stride)))
         (res (make-list n :initial-element (make-list m :initial-element 0))))
    (dotimes (i n)
      (dotimes (j m)
        (dotimes (k kernel-size)
          (dotimes (l kernel-size)
            (incf (nth j (nth i res)) 
                  (nth (+ (* i stride) k) (nth (+ (* j stride) l) t1-data)))))))
    (dotimes (i n)
      (dotimes (j m)
        (setf (nth j (nth i res)) 
              (/ (nth j (nth i res)) (* kernel-size kernel-size)))))
    (let* ((final-data res)
           (final-shape (list n m)))
      (make-tensor final-data final-shape))))
