;; Load Packages
(load "nn_package.lisp")

(in-package :lispgrad/nn)

;; non-linear activation functions
;; implementations derived from: https://pytorch.org/docs/stable/nn.functional.html

;; description:
;;  (relu (t1)): Computes the Rectified Linear Unit activation function on a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor with the same shape as the input, where each element is the ReLU applied on the input element.
;; example:
;;  (relu '((-1 2 -3) (4 -5 7))) ; returns '((0 2 0) (4 0 7))
(defun relu (t1)
  (make-tensor (mapcar #'(lambda (x) (max 0 x)) (tensor-data t1)) (tensor-shape t1)))


;; description:
;;  (relu6 (t1)): Computes the Rectified Linear Unit activation function on a tensor with a vertical asymptote at y = 6.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor with the same shape as the input, where each element is the ReLU applied on the input element with asympotote at y = 6.
;; example:
;;  (relu '((-1 2 -3) (4 -5 7))) ; returns '((0 2 0) (4 0 6))
(defun relu-6 (t1)
  (make-tensor (mapcar #'(lambda (x) (min 6 (max 0 x))) (tensor-data t1)) (tensor-shape t1)))


;; description:
;;  (celu (t1 alpha)): Computes CELU element-wise on a tensor, given a parametric alpha value
;; requires:
;;  t1: valid tensor
;;  alpha: Float
;; returns:
;;  A tensor with the same shape as the input, where each element has the computed CELU value.
;; example:
;;  (celu '(-1 0 1 7) 1) ; returns '(-0.63212055 0 1 7)
(defun celu (t1 alpha)
  (make-tensor (mapcar #'(lambda (x) (+ (max 0 x) (min 0 (* alpha (- (exp (/ x alpha)) 1))))) 
    (tensor-data t1)) (tensor-shape t1)))


;; description:
;;  (clip (t1 min-val max-val)): Clips the elements of a tensor to be within a range specified by min-val and max-val.
;; requires:
;;  t1: valid tensor
;;  min-val: minimum value to clip the tensor elements to
;;  max-val: maximum value to clip the tensor elements to
;; returns:
;;  A tensor with the same shape as the input, where each element is clipped to lie within min-val and max-val.
;; example:
;;  (clip '((2 4 6) (8 10 12)) 3 9) ; returns '((3 4 6) (8 9 9))
(defun clip (t1 min-val max-val)
  (assert (<= min-val max-val))
  (let* ((t-data (tensor-data t1))
         (t-shape (tensor-shape t1))
         (clipped-data (mapcar #'(lambda (x) (min max-val (max min-val x))) t-data)))
    (make-tensor clipped-data t-shape)))


;; description:
;;  (hard-tanh (t1 min-val max-val)): Applies the HardTanh activation function on a tensor, with specified minimum and maximum values.
;; requires:
;;  t1: valid tensor
;;  min-val: minimum value to clip the tensor elements to after applying the tanh function
;;  max-val: maximum value to clip the tensor elements to after applying the tanh function
;; returns:
;;  A tensor with the same shape as the input, where each element is the result of applying HardTanh to the corresponding input element.
;; example:
;;  (hard-tanh '((0.5 0.2 -0.8) (0.1 -0.4 0.6)) -1 1) ; returns '((0.5 0.2 -0.8) (0.1 -0.4 0.6))
(defun hard-tanh (t1 min-val max-val)
  (assert (<= min-val max-val))
  (clip t1 min-val max-val))


;; description:
;;  (leaky-relu (t1 alpha)): Applies the Leaky Rectified Linear Unit activation function on a tensor.
;; requires:
;;  t1: valid tensor
;;  alpha: a small positive number (typically 0.01)
;; returns:
;;  A tensor with the same shape as the input, where each element is the Leaky ReLU applied on the input element.
;; example:
;;  (leaky-relu '((1 -2 3) (-4 5 -6)) 0.01) ; returns '((1 -0.02 3) (-0.04 5 -0.06))
(defun leaky-relu (t1 alpha)
  (make-tensor (mapcar #'(lambda (x) (if (> x 0) x (* alpha x))) (tensor-data t1)) (tensor-shape t1)))
