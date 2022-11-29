all : compiler

compiler : src/Main.hs src/Utils.hs syntax typechecker optimizer
	ghc -isrc src/Main.hs -o latc

syntax : src/Syntax/AbsLattepp.hs src/Syntax/LexLattepp.hs src/Syntax/ParLattepp.hs

optimizer : src/Optimizer/Optimizer.hs src/Optimizer/Data.hs src/Optimizer/Utils.hs

typechecker : src/Typechecker/TypeChecker.hs src/Typechecker/Data.hs src/Typechecker/Utils.hs

# compiler : Compiler/Compiler.hs Compiler/CompilerData.hs 

clean :
	-rm -f src/*.hi src/*.o latc
	-rm -f src/Optimizer/*.o src/Optimizer/*.hi
	-rm -f src/Typechecker/*.o src/Typechecker/*.hi
	-rm -f src/Compiler/*.o src/Compiler/*.hi
	-make -C src/Syntax clean
	-rm -f src/Syntax/*.y src/Syntax/*.hi src/Syntax/*.bak src/Syntax/*.info src/Syntax/*.x