all : compiler

compiler : Main.hs Utils.hs syntax typechecker optimizer
	ghc Main.hs -o latc

syntax : Syntax/AbsLattepp.hs Syntax/LexLattepp.hs Syntax/ParLattepp.hs

optimizer : Optimizer/Optimizer.hs Optimizer/OptimizerData.hs

typechecker : Typechecker/TypeChecker.hs Typechecker/TypeCheckerData.hs

# compiler : Compiler/Compiler.hs Compiler/CompilerData.hs 

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi interpreter
	-rm -f Optimizer/*.o Evaluator/*.hi
	-rm -f Typechecker/*.o Typechecker/*.hi
	-make -C Syntax clean