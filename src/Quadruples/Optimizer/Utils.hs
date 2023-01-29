module Quadruples.Optimizer.Utils where
import           Quadruples.Data


basicBlockBoundry :: Quadruple -> Bool
basicBlockBoundry (Label _) = True
basicBlockBoundry (Jmp _)   = True
basicBlockBoundry (Je  _)   = True
basicBlockBoundry (Jne _)   = True
basicBlockBoundry (Jge _)   = True
basicBlockBoundry (Jg  _)   = True
basicBlockBoundry (Jle _)   = True
basicBlockBoundry (Jl  _)   = True
basicBlockBoundry _         = False

extractIndex :: Quadruple -> Maybe QIndex
extractIndex q = case q of
  Inc qi       -> Just qi
  Dec qi       -> Just qi
  Load qi _    -> Just qi
  LoadArg _ qi -> Just qi
  Store _ qi   -> Just qi
  _            -> Nothing
