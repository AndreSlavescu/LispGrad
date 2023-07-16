;; Load Packages
(load "../tensor.lisp")

;; nn package
(defpackage :lispgrad/nn
  (:nicknames :lispgrad.nn)
  (:use :common-lisp :lispgrad)
  (:export 
    ;; binary ops
    #:tensor-add 
    #:tensor-sub 
    #:tensor-mul 
    #:tensor-div
    #:tensor-land
    #:tensor-lior
    #:tensor-lxor
    
    ;; mat_mul
    #:transpose
    #:mat-mul
    
    ;; conv
    #:conv-2d))