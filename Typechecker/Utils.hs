module Typechecker.Utils where
import Syntax.AbsLattepp (BNFC'Position, Type, Expr, Arg, Ident, HasPosition (hasPosition), Type' (Primitive))
import Typechecker.Data (TypeCheckerState, TypeCheckerS, TypeCheckerException (..), setTypes, emptyScope, setExpectedReturnType)
import Utils (Raw(raw), throwException, firstDuplicateIndex, getArgIdent, rawVoid, rawFun, getArgType)
import Control.Monad (zipWithM_, when)

