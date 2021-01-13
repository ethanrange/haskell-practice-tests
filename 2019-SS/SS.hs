module SOL where

import Data.List
import Data.Maybe
import Data.Tuple

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

-- Blackbird operator
(...) = (.) . (.)

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp = fromJust ... lookup

-- 3 marks
vars :: Formula -> [Id]
vars = sort . nub . v
  where
    v (Var i)    = [i]
    v (Not f)    = v f
    v (And f f') = v f ++ v f'
    v (Or f f')  = v f  ++ v f'

-- 1 mark
idMap :: Formula -> IdMap
idMap f = zip (vars f) [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not f) = case f of
  And f1 f2 -> toNNF (Or (Not f1) (Not f2))
  Or f1 f2  -> toNNF (And (Not f1) (Not f2))
  Not f'    -> toNNF f'
  _         -> Not (toNNF f)
  where
    nf = toNNF f
toNNF (And f1 f2) = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2)  = Or (toNNF f1) (toNNF f2)
toNNF (Var v)     = Var v

-- 3 marks
toCNF :: Formula -> CNF
toCNF = tcnf . toNNF
  where
    tcnf (Or f1 f2) = distribute (tcnf f1) (tcnf f2)
    tcnf f          = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f = flt f
  where
    m = idMap f
    flt :: CNF -> CNFRep
    flt (And f1 f2@(And _ _)) = fc f1 : flt f2
    flt (And f1@(And _ _) f2) = flt f1 ++ [fc f2]
    flt (And f1 f2)           = [fc f1, fc f2]
    flt f                     = [fc f]
    
    fc (Var i)       = [lookUp i m]
    fc (Not (Var i)) = [-(lookUp i m)]
    fc (Or f1 f2)    = fc f1 ++ fc f2

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits f
  | null ss   = (f, [])
  | otherwise = (f', s : is)
  where
    ss = [x | [x] <- f]
    s  = head ss

    (f', is) = propUnits cld
    cld = filter (notElem s) (map (\\ [-s]) f)
    


-- 4 marks
dp :: CNFRep -> [[Int]]
dp f
  | elem [] f' = []
  | otherwise  = case f' of
    []            -> [is]
    ((u : _) : _) -> map (is ++) (dp ([u] : f')) ++ map (is ++) (dp ([-u] : f'))
  where
    (f', is) = propUnits f


--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
--allSat :: Formula -> [[(Id, Bool)]]
allSat f = map (map pair) rs
  where
    sols = dp . flatten . toCNF
    ids  = map swap (idMap f)
    rs   = concatMap (completeResults 1 (length ids)) (sols f)
    pair v | v > 0     = (lv, True)
           | otherwise = (lv, False)
      where lv = lookUp (abs v) ids

completeResults :: Int -> Int -> [Int] -> [[Int]]
completeResults n max rs
  | n > max = [[]]
  | elem n rs    = map (n :) (completeResults (n + 1) max rs)
  | elem (-n) rs = map ((-n) :) (completeResults (n + 1) max rs)
  | otherwise    = completeResults n max (n : rs) ++ completeResults n max ((-n) : rs)