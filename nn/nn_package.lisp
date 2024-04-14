;; Load Packages
(load (merge-pathnames "../config.lisp" *load-truename*))
(load (merge-pathnames "tensor.lisp" *base-dir*))

;; nn package
(defpackage :lispgrad/nn
  (:nicknames :lispgrad.nn)
  (:use :common-lisp :lispgrad)
  (:export 
    ;; unary ops
    #:tensor-neg
    #:tensor-abs
    #:tensor-exp
    #:tensor-log

    ;; binary ops
    #:tensor-add 
    #:tensor-sub 
    #:tensor-mul 
    #:tensor-div
    #:tensor-land
    #:tensor-lior
    #:tensor-lxor
    
    ;; non-linear activations
    #:relu
    #:relu6
    #:celu
    #:clip
    #:hard-tanh
    #:gelu
    #:tanh-activation
    #:sigmoid

    ;; matrix ops
    #:transpose
    #:mat-mul
    
    ;; conv
    #:conv-2d
    
    ;; pool
    #:avg-pool-2d
    
    ;; attention
    #:scaled-dot-product-attention
    ))
  