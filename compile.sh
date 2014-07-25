#!/bin/bash

if [ -e "$1" ]; then
    sbcl --noinform --load compile.lisp \
	--eval \(compile-program\ \"$1\"\) \
	--eval "(quit)" 2> /dev/null
fi

