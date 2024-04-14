;; non_linear_activations.lisp

;; Load Packages
(load (merge-pathnames "../config.lisp" *load-truename*))
(in-package :lispgrad/utils)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))
(ql:quickload "special-functions")


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
;;  (relu6 '((-1 2 -3) (4 -5 7))) ; returns '((0 2 0) (4 0 6))
(defun relu6 (t1)
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


;; description:
;;  (gelu (t1)): Applies the Gaussian Error Linear Unit activation function on a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor with the same shape as the input, where each element is the GELU applied on the input element.
;; example:
;;  (gelu '((-1 0 1) (2 3 4))) ; approximate results '((-0.1588 0 0.8412) (1.9546 2.9963 3.9993))
(defun gelu (t1)
  (make-tensor (mapcar #'(lambda (x) (* x 0.5 (+ 1 (special-functions:erf (/ (coerce x 'double-float) (sqrt 2.0)))))) (tensor-data t1)) (tensor-shape t1)))


;; description:
;;  (tanh-activation (t1)): Applies the hyperbolic tangent activation function on a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor with the same shape as the input, where each element is the tanh applied on the input element.
;; example:
;;  (tanh-activation '((-1 0 1) (2 3 4))) ; returns '((0.7616 0 0.7616) (0.9640 0.9951 0.9993))
(defun tanh-activation (t1)
  (make-tensor (mapcar #'tanh (tensor-data t1)) (tensor-shape t1)))


;; description:
;;  (sigmoid (t1)): Applies the sigmoid activation function on a tensor.
;; requires:
;;  t1: valid tensor
;; returns:
;;  A tensor with the same shape as the input, where each element is the sigmoid applied on the input element.
;; example:
;;  (sigmoid '((-1 0 1) (2 3 4))) ; returns '((0.2689 0.5 0.7311) (0.8808 0.9526 0.9820))
(defun sigmoid (t1)
  (make-tensor (mapcar #'(lambda (x) (/ 1 (+ 1 (exp (- x))))) (tensor-data t1)) (tensor-shape t1)))
