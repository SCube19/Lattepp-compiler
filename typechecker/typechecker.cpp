#include "typechecker.h"

#include <memory>

// PROGRAM
void typechecker::typecheck(const Prog& program) {}

namespace {
// DEFINITIONS
void typecheck(const FnDef& def) {}

void typecheck(const ClassDef& def) {}

void typecheck(const ExtClassDef& def) {}

// ARGUMENT
void typecheck(const Ar& arg) {}

// BLOCKS
void typecheck(const Bloc& block) {}
void typecheck(const ClassBloc& block) {}

// CLASS STATEMENT
void typecheck(const ClassEmpty& stmt) {}
void typecheck(const ClassDecl& stmt) {}
void typecheck(const ClassMethod& stmt) {}

// STATEMENT
void typecheck(const Empty& stmt) {}
void typecheck(const BStmt& stmt) {}
void typecheck(const Decl& stmt) {}

void typecheck(const Ass& stmt) {}
void typecheck(const Incr& stmt) {}
void typecheck(const Decr& stmt) {}
void typecheck(const Ret& stmt) {}
void typecheck(const VRet& stmt) {}
void typecheck(const Cond& stmt) {}
void typecheck(const CondElse& stmt) {}
void typecheck(const While& stmt) {}
void typecheck(const For& stmt) {}
void typecheck(const SExp& stmt) {}

// ITEM
void typecheck(const NoInit& item) {}
void typecheck(const Init& item) {}

// EXT IDENT
void typecheck(const Id& ident) {}
void typecheck(const ArrId& ident) {}
void typecheck(const AttrId& ident) {}

// EXPRESSION
std::unique_ptr<Type> typecheckExpr(const ECast& e) {}
std::unique_ptr<Type> typecheckExpr(const ECastPrim& e) {}
std::unique_ptr<Type> typecheckExpr(const ENewObject& e) {}
std::unique_ptr<Type> typecheckExpr(const ENewArr& e) {}
std::unique_ptr<Type> typecheckExpr(const ENull& e) {}
std::unique_ptr<Type> typecheckExpr(const EObject& e) {}
std::unique_ptr<Type> typecheckExpr(const EArr& e) {}
std::unique_ptr<Type> typecheckExpr(const EVar& e) {}
std::unique_ptr<Type> typecheckExpr(const ELitInt& e) {}
std::unique_ptr<Type> typecheckExpr(const ELitTrue& e) {}
std::unique_ptr<Type> typecheckExpr(const ELitFalse& e) {}
std::unique_ptr<Type> typecheckExpr(const EApp& e) {}
std::unique_ptr<Type> typecheckExpr(const EString& e) {}
std::unique_ptr<Type> typecheckExpr(const Neg& e) {}
std::unique_ptr<Type> typecheckExpr(const Not& e) {}
std::unique_ptr<Type> typecheckExpr(const EMul& e) {}
std::unique_ptr<Type> typecheckExpr(const EAdd& e) {}
std::unique_ptr<Type> typecheckExpr(const ERel& e) {}
std::unique_ptr<Type> typecheckExpr(const EAnd& e) {}
std::unique_ptr<Type> typecheckExpr(const EOr& e) {}
}  // namespace
