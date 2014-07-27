#!/bin/bash

if [ -e "$1" ]; then
    cat lib.lisp game.lisp $1 $2 > /tmp/tmp.lisp
    if [ "$2" == "nolibs" ]; then
	cp $1 /tmp/tmp.lisp
    fi
    sbcl --noinform --load compile.lisp \
	--eval \(compile-program\ \"/tmp/tmp.lisp\"\) \
	--eval "(quit)" 2> /dev/null
fi

