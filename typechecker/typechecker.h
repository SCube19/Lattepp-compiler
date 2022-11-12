#ifndef TYPECHECKER_H
#define TYPECHECKER_H

#include "../syntax/Absyn.H"

namespace typechecker {
void typecheck(const Prog& program);
}

#endif