#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo "Running Tests!"

sbcl --script "$SCRIPT_DIR/../config.lisp"
sbcl --script "$SCRIPT_DIR/../utils/utils_package.lisp"
sbcl --script "$SCRIPT_DIR/nn_package.lisp"
sbcl --script "$SCRIPT_DIR/tests/test_unary_ops.lisp"
# sbcl --script "$SCRIPT_DIR/tests/test_binary_ops.lisp"
# sbcl --script "$SCRIPT_DIR/tests/test_matrix_ops.lisp"
# sbcl --script "$SCRIPT_DIR/tests/test_conv2d.lisp"
# sbcl --script "$SCRIPT_DIR/tests/test_avg_pool2d.lisp"
# sbcl --script "$SCRIPT_DIR/tests/test_non_linear_activations.lisp"

echo "All tests executed."
