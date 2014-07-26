all:
	bash compile.sh clockwork.lisp | xclip

submit:
	mkdir -p staging/code
	mkdir -p staging/solutions
	bash compile.sh clockwork-cers.ll einars.lisp \
		> staging/solutions/lambdaman.gcc
	cp *.lisp compile.sh Makefile staging/code
	mv staging raging-mushrooms
	tar cfz raging-mushrooms.tgz raging-mushrooms
	rm -rf raging-mushrooms
