#!/bin/bash

# run tests

printf "Running Tests!\n"

sbcl --script test_binary_ops.lisp
sbcl --script test_mat_mul.lisp
sbcl --script test_conv2d.lisp
sbcl --script test_avg_pool2d.lisp
sbcl --script test_non_linear_activations.lisp

printf "\n\nAll Tests Passed!\n"
