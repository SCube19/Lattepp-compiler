CC=g++ -g
CCFLAGS= --std=c++2a -W -Wall -pedantic -Wsign-conversion -Wno-unused-parameter -Wno-unused-function
SYNTAX_OBJ=syntax/Parser.o syntax/Absyn.o syntax/Printer.o syntax/Buffer.o syntax/Lexer.o
SYNTAX_FILES=syntax/Absyn.C syntax/Absyn.H syntax/Bison.H syntax/Buffer.C syntax/Buffer.H syntax/Lexer.C\
	syntax/Parser.C syntax/Parser.H syntax/ParserError.H syntax/Printer.C syntax/Printer.H syntax/Skeleton.C\
	syntax/Skeleton.H syntax/Test.C syntax/Makefile
TYPECHECKER_FILES=typechecker/typechecker.h typechecker/typechecker.cpp typechecker/structures.h

all: main syntaxTarget typechecker utils 
	${CC} ${CCFLAGS} main.o utils.o typechecker/typechecker.o ${SYNTAX_OBJ} -o latc

main: main.cpp syntaxTarget
	${CC} ${CCFLAGS} -c main.cpp -o main.o

utils: utils.h utils.cpp
	${CC} ${CCFLAGS} -c utils.cpp -o utils.o

typechecker: ${TYPECHECKER_FILES} syntaxTarget
	${CC} ${CCFLAGS} -c typechecker/typechecker.cpp -o typechecker/typechecker.o

syntaxTarget: ${SYNTAX_FILES}
	cd syntax && make && cd ..

clean:
	rm -r *.o && cd syntax && make clean && cd ..