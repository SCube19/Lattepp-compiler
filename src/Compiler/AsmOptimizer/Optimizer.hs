module Compiler.AsmOptimizer.Optimizer where
import           Compiler.Data (AsmInstr (Add, Clear, Cmp, Div, Lea, Mov, Mul, Neg, Not, Sub),
                                AsmOperand (RegOp, ValOp),
                                AsmRegister (RAX, RBX, RCX, RDX),
                                AsmValue (VInt), OpSize (QWord), isAsmReg,
                                isAsmValue)

optimizeAsm :: [AsmInstr] ->  [AsmInstr]
optimizeAsm is =
    let newCode = (sameInstr . reversedMoves . pointless) is in
    if newCode == is then
        (deadAsm . cmpOpt) newCode
    else
        optimizeAsm newCode

pointless :: [AsmInstr] -> [AsmInstr]
pointless [] = []
pointless (m@(Mov _ op1 op2):is) = if op1 == op2 then pointless is else m : pointless is
pointless ((Add _ _ (ValOp (VInt 0))):is) = pointless is
pointless ((Sub _ _ (ValOp (VInt 0))):is) = pointless is
pointless ((Mul _ _ (ValOp (VInt 1))):is) = pointless is
pointless ((Div _ (ValOp (VInt 1))):is) = Mov QWord (RegOp RDX) (ValOp $ VInt 0) : pointless is
pointless (i:is) = i : pointless is

reversedMoves :: [AsmInstr] -> [AsmInstr]
reversedMoves []     = []
reversedMoves (i:is) = _reversedMoves is i [i]

_reversedMoves  :: [AsmInstr] -> AsmInstr -> [AsmInstr] -> [AsmInstr]
_reversedMoves [] _ acc = reverse acc
_reversedMoves (i:is) prev acc =
    case i of
        Mov _ op1 op2 ->
            case prev of
                Mov _ op1prev op2prev ->
                    if  op1 == op2prev && op2 == op1prev then
                        _reversedMoves is prev acc
                    else
                        _reversedMoves is i (i : acc)
                _ -> _reversedMoves is i (i : acc)
        _ -> _reversedMoves is i (i : acc)

cmpOpt :: [AsmInstr] -> [AsmInstr]
cmpOpt []     = []
cmpOpt (i:is) = _cmpOpt is i [i]

_cmpOpt  :: [AsmInstr] -> AsmInstr -> [AsmInstr] -> [AsmInstr]
_cmpOpt [] _ acc = reverse acc
_cmpOpt (i:is) prev acc =
    case i of
        (Cmp s op1 op2) ->
            case prev of
                (Mov _ op1prev op2prev) ->
                    if (isAsmReg op2 || isAsmValue op2) && op2 == op1prev then
                        _cmpOpt is i (Cmp s op1 op2prev : tail acc)
                    else
                        _cmpOpt is i (i : acc)
                _ -> _cmpOpt is i (i : acc)
        _ -> _cmpOpt is i (i : acc)


sameInstr :: [AsmInstr] -> [AsmInstr]
sameInstr []     = []
sameInstr (i:is) = _sameInstr is i [i]

_sameInstr :: [AsmInstr] -> AsmInstr -> [AsmInstr] -> [AsmInstr]
_sameInstr [] _ acc = reverse acc
_sameInstr (i:is) prev acc =
    if legalSame prev && legalSame i && i == prev then
        _sameInstr is prev acc
    else
        _sameInstr is i (i : acc)

legalSame :: AsmInstr -> Bool
legalSame (Cmp _ o1 o2) = True
legalSame (Lea s o1 o2) = True
legalSame (Mov s o1 o2) = True
legalSame (Clear s o1)  = True
legalSame _             = False



deadAsm :: [AsmInstr] -> [AsmInstr]
deadAsm is = is

