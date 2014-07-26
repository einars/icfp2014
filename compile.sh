#!/bin/bash

if [ -e "$1" ]; then
    cat lib.lisp game.lisp $1 > /tmp/tmp.lisp
    sbcl --noinform --load compile.lisp \
	--eval \(compile-program\ \"/tmp/tmp.lisp\"\) \
	--eval "(quit)" 2> /dev/null
fi

