;; Load Packages
(load (merge-pathnames "../config.lisp" *load-truename*))
(load (merge-pathnames "tensor.lisp" *base-dir*))

;; nn package
(defpackage :lispgrad/utils
  (:nicknames :lispgrad.utils)
  (:use :common-lisp :lispgrad)
  (:export 
    ;; assertion tools
    assert-close
    ))
