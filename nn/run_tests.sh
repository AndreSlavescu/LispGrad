#!/bin/bash

# run tests
sbcl --script test_binary_ops.lisp
sbcl --script test_mat_mul.lisp
sbcl --script test_conv2d.lisp

