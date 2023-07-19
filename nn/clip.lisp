;; Load Packages
(load "nn_package.lisp")

(in-package :lispgrad/nn)


(defun clip (t1 min-val max-val)
  (let* ((t-data (tensor-data t1))
         (t-shape (tensor-shape t1))
         (clipped-data (mapcar #'(lambda (x) (min max-val (max min-val x))) t-data)))
    (make-tensor clipped-data t-shape)))
