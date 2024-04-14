;; assertion_tools.lisp
(load (merge-pathnames "../config.lisp" *load-truename*))
(in-package :lispgrad/utils)
(load (merge-pathnames "utils_package.lisp" *utils-dir*))


;; assert-close function definition
(defun assert-close (x y &optional (tolerance 0.0001))
  "Compares two numbers (x and y) and checks if they are within a specified tolerance.
  
  Parameters:
  - x: The first number to compare.
  - y: The second number to compare.
  - tolerance: The maximum allowed difference between x and y for them to be considered 'close enough' (default is 0.0001).
  
  Returns:
  - T if x and y are within the specified tolerance, NIL otherwise."
  (and (numberp x) (numberp y)
       (<= (abs (- x y)) tolerance)))
