(defparameter *base-dir* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *utils-dir* (merge-pathnames "utils/" *base-dir*))
(defparameter *nn-dir* (merge-pathnames "nn/" *base-dir*))
