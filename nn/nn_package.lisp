(load "../tensor.lisp")

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
    
    ;; mat_mul
    #:transpose
    #:mat-mul))