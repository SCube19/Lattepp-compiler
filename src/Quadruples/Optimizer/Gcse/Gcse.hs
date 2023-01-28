module Quadruples.Optimizer.Gcse.Gcse where
import           Quadruples.Data                (Quadruple)
import           Quadruples.Optimizer.Gcse.Data (GcseState)


gcse :: [Quadruple] -> GcseState [Quadruple]
gcse qs = return qs
