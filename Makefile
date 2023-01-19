all : compiler

compiler : src/Main.hs src/Utils.hs syntax typechecker optimizer
	i686-linux-gnu-gcc -c lib/runtime.c -o lib/runtime.o
	ghc -isrc -package ghc src/Main.hs -o latc_x86 
	cp src/latc latc
	chmod +x latc
	
syntax : src/Syntax/AbsLattepp.hs src/Syntax/LexLattepp.hs src/Syntax/ParLattepp.hs

optimizer : src/Optimizer/Optimizer.hs src/Optimizer/Data.hs src/Optimizer/Utils.hs

typechecker : src/Typechecker/TypeChecker.hs src/Typechecker/Data.hs src/Typechecker/Utils.hs

compiler : src/Compiler/Compiler.hs src/Compiler/Data.hs src/Compiler/Quadruples/Quadruples.hs src/Compiler/Quadruples/Data.hs 

clean :
	-rm lib/runtime.o
	-rm -f src/*.hi src/*.o latc_x86 latc
	-rm -f src/Optimizer/*.o src/Optimizer/*.hi
	-rm -f src/Typechecker/*.o src/Typechecker/*.hi
	-rm -f src/Compiler/*.o src/Compiler/*.hi
	-rm -f src/Compiler/Quadruples/*.o src/Compiler/Quadruples/*.hi
	-make -C src/Syntax clean
	-rm -f src/Syntax/*.y src/Syntax/*.hi src/Syntax/*.bak src/Syntax/*.info src/Syntax/*.x