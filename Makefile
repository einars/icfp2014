all:
	bash compile.sh clockwork.lisp | xclip

submit:
	rm -rf staging
	rm -rf raging-mushrooms
	mkdir -p staging/code/lambdaman
	mkdir -p staging/code/ghostman
	mkdir -p staging/code/asm-compile
	mkdir -p staging/solution
	bash compile.sh clockwork.lisp > staging/solution/lambdaman.gcc
	cp clockwork.lisp game.lisp lib.lisp preprocess-pils.lisp compile.sh Makefile staging/code/lambdaman
	cp    ml.asm-compile/Makefile staging/code/asm-compile
	cp -r ml.asm-compile/src      staging/code/asm-compile
	cp    ml.asm-compile/ghostman.asm staging/code/ghostman/ghostman.asm
	ml.asm-compile/asm_compile.native ml.asm-compile/ghostman.asm > staging/solution/ghost0.ghc
	mv staging raging-mushrooms
	tar cfz raging-mushrooms.release.tgz raging-mushrooms
	sha1sum raging-mushrooms.release.tgz
	#rm -rf raging-mushrooms

