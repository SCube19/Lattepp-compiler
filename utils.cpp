#include "utils.h"

#include <cstdlib>
#include <iostream>

void utils::usage() {
    std::cout << "Usage ./latc <file>\n";
    exit(EXIT_FAILURE);
}

void utils::typecheckingError(const std::string& msg, int lineNumber,
                              int column) {
    std::cerr << msg << " on line " << lineNumber << " (column " << column
              << ")";
    exit(EXIT_FAILURE);
}