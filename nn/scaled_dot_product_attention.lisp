;; scaled_dot_product_attention.lisp

;; Load Packages
(load (merge-pathnames "../config.lisp" *load-truename*))
(in-package :lispgrad/utils)
(load (merge-pathnames "nn_package.lisp" *nn-dir*))


;; description:
;;  (scaled-dot-product-attention Q K V &optional mask): Computes the scaled dot-product attention.
;; requires:
;;  Q: Query matrix
;;  K: Key matrix, must have the same dimension as Q for dot product.
;;  V: Value matrix, dimensionality can differ from Q and K.
;;  mask (optional): Matrix used to exclude certain positions from attention.
;; returns:
;;  attention_output: The weighted sum of the values based on computed attention scores.
;;  attention_weights: The attention scores after applying softmax.
;; example:
;;  (scaled-dot-product-attention Q K V mask) ; returns (attention_output, attention_weights)
(defun scaled-dot-product-attention (Q K V &optional mask))
