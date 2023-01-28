all: build

build: base src/Main.hs abstract quadruples compiler
	gcc -c -g lib/runtime.c -o lib/runtime.o
	ghc -isrc -package ghc src/Main.hs -o latc_x86-64
	cp src/latc latc
	chmod +x latc

compiler: base allocator asmoptimizer src/Compiler/Compiler.hs src/Compiler/Data.hs

allocator: base src/Compiler/Allocator/Allocator.hs src/Compiler/Allocator/Data.hs 

asmoptimizer: base src/Compiler/Optimizer/Optimizer.hs

quadruples: base quadoptimizer src/Quadruples/Data.hs src/Quadruples/Quadruples.hs src/Quadruples/Predata.hs src/Quadruples/Preprocess.hs

quadoptimizer: base 

abstract: base absoptimizer syntax typechecker

absoptimizer: base src/Abstract/Optimizer/Data.hs src/Abstract/Optimizer/Optimizer.hs src/Abstract/Optimizer/Utils.hs

typechecker: base src/Abstract/Typechecker/TypeChecker.hs src/Abstract/Typechecker/Data.hs src/Abstract/Typechecker/Utils.hs

syntax: base src/Syntax/AbsLattepp.hs src/Syntax/LexLattepp.hs src/Syntax/ParLattepp.hs

base: src/Utils.hs

clean :
	-find . -name "*.o" -delete
	-find . -name "*.hi" -delete
	-find . -name "*.y" -delete
	-find . -name "*.info" -delete
	-find . -name "*.bak" -delete
	-find . -name "*.x" -delete
	-rm -f latc_x86-64 latc
	-make -C src/Syntax clean