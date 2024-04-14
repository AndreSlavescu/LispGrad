#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo "Running Tests!"

sbcl --noinform <<EOF
(load "$SCRIPT_DIR/../config.lisp")   
(load "$SCRIPT_DIR/../utils/utils_package.lisp)
(load "$SCRIPT_DIR/nn_package.lisp") 

;; Load and run tests
(load "$SCRIPT_DIR/tests/test_unary_ops.lisp")
(load "$SCRIPT_DIR/tests/test_binary_ops.lisp")
(load "$SCRIPT_DIR/tests/test_matrix_ops.lisp")
(load "$SCRIPT_DIR/tests/test_conv2d.lisp")
(load "$SCRIPT_DIR/tests/test_avg_pool2d.lisp")
(load "$SCRIPT_DIR/tests/test_non_linear_activations.lisp")

;; Final output
(format t "~%All Tests Passed!~%")
(sb-ext:quit)
EOF

echo "All tests executed."
