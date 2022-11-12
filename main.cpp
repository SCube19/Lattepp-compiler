#include <cstdio>
#include <fstream>
#include <memory>
#include <streambuf>
#include <string>

#include "syntax/Absyn.H"
#include "syntax/Buffer.H"
#include "syntax/Parser.H"
#include "syntax/Printer.H"
#include "syntax/Skeleton.H"
#include "typechecker/typechecker.h"
#include "utils.h"

int main(int argc, char** argv) {
    if (argc != 2) utils::usage();

    FILE* file = fopen(argv[1], "r");
    Prog program(*dynamic_cast<Prog*>(pProgram(file)));
    fclose(file);

    typechecker::typecheck(program);

    return 0;
}