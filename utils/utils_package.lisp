;; Load Packages
(load "../tensor.lisp")

;; nn package
(defpackage :lispgrad/utils
  (:nicknames :lispgrad.utils)
  (:use :common-lisp :lispgrad)
  (:export 
    ;; assertion tools
    assert-close
    ))
