import Data.Maybe
import Data.List

type Id = String

type Function = (Id, [Id], Block)

type Block = [Statement]

data Statement = Assign Id Exp |
                 If Exp Block Block |
                 DoWhile Block Exp 
               deriving (Eq, Show)

data Exp = Const Int | Var Id | Apply Op Exp Exp | Phi Exp Exp
         deriving (Eq, Show)

data Op = Add | Mul | Eq | Gtr 
        deriving (Eq, Show)

-- Blackbird Operators
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) = (.) . (.) . (.)
------------------------------------------------------------------------
-- Given functions to support the interpreter...

lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp i table
  = fromMaybe (error ("lookup failed on identifier: " ++ show i)) 
              (lookup i table) 

execFun :: Function -> [Int] -> State
execFun (name, args, p) vs
  = execBlock p (zip args vs)

------------------------------------------------------------------------
-- Part I

type State = [(Id, Int)]

update :: (Id, Int) -> State -> State
update p@(id, _) s = p : filter ((/=id) . fst) s

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Mul = (*)
apply Eq  = fromEnum .: (==)
apply Gtr = fromEnum .: (>)

eval :: Exp -> State -> Int
-- Pre: the variables in the expression will all be bound in the given state 
-- Pre: expressions do not contain phi instructions
eval (Const c) _       = c
eval (Var id) s        = lookUp id s
eval (Apply o e1 e2) s = apply o (eval e1 s) (eval e2 s)

execStatement :: Statement -> State -> State
execStatement (Assign id e) s = update (id, eval e s) s
execStatement (If p b1 b2) s
  | eval p s == 1 = execBlock b1 s
  | otherwise     = execBlock b2 s
execStatement (DoWhile b p) s
  | eval p ns == 1 = execStatement (DoWhile b p) ns
  | otherwise      = ns
  where
    ns = execBlock b s

execBlock :: Block -> State -> State
execBlock = flip (foldl (flip execStatement))

------------------------------------------------------------------------
-- Given function for testing propagateConstants...

-- Converts a function in SSA form into its optimised SSA form...
applyPropagate :: Function -> Function
applyPropagate (name, args, body)
  = (name, args, propagateConstants body)

------------------------------------------------------------------------
-- PART II

foldConst :: Exp -> Exp
-- Pre: the expression is in SSA form
foldConst (Phi c c')
  | c == c' = c
foldConst (Apply Add c (Const 0))        = c
foldConst (Apply Add (Const 0) c)        = c
foldConst (Apply o (Const c) (Const c')) = Const (apply o c c')
foldConst e                              = e

sub :: Id -> Int -> Exp -> Exp
-- Pre: the expression is in SSA form
sub = foldConst ... s
  where
    s id v (Phi c c')      = Phi (sub id v c) (sub id v c')
    s id v (Apply o e1 e2) = Apply o (sub id v e1) (sub id v e2)
    s id v (Var t)
      | id == t = Const v
    s _ _ e                = e

-- Use (by uncommenting) any of the following, as you see fit...
type Worklist = [(Id, Int)]

scan :: Id -> Int -> Block -> (Worklist, Block)
scan id n b = foldr scan' ([], []) b
  where
    scan' (Assign "$return" e) (wl, b) = (wl, Assign "$return" e : b)
    scan' (Assign v e) (wl, b) = case sub id n e of
      Const c' -> ((v, c') : wl, b)
      t        -> (wl, Assign v t : b)
    scan' (If p b1 b2) (wl, b) = (wl ++ wl1 ++ wl2, If (sub id n p) nb1 nb2 : b)
      where 
        (wl1, nb1) = scan id n b1
        (wl2, nb2) = scan id n b2
    scan' (DoWhile b1 p) (wl, b) = (wl ++ wl1, DoWhile nb1 (sub id n p) : b)
      where
        (wl1, nb1) = scan id n b1
 
propagateConstants :: Block -> Block
-- Pre: the block is in SSA form
propagateConstants b = snd (pc il ib)
  where
    (il, ib) = scan "$INVALID" 0 b
    pc :: Worklist -> Block -> (Worklist, Block)
    pc [] b' = ([], b')
    pc ((v, c) : vs) b' = pc (vs ++ nwl) nb
      where
        (nwl, nb) = scan v c b'

------------------------------------------------------------------------
-- Given functions for testing unPhi...

-- Applies unPhi to a given function...
applyUnPhi :: Function -> Function
applyUnPhi (name, args, body)
  = (name, args, unPhi body)

-- Combines propagation/folding and unPhi to convert a function from its
-- unoptimised SSA form to a final non-SSA form...
optimise :: Function -> Function
optimise (name, args, body)
  = (name, args, unPhi (propagateConstants body))

------------------------------------------------------------------------
-- PART III

unPhi :: Block -> Block
-- Pre: the block is in SSA form
unPhi [] = []
unPhi (If p b1 b2 : Assign v (Phi e1 e2) : bs) = unPhi (If p nb1 nb2 : bs)
  where
    (nb1, nb2) = (b1 ++ [Assign v e1], b2 ++ [Assign v e2])
unPhi (If p b1 b2 : bs) = If p (unPhi b1) (unPhi b2) : unPhi bs
unPhi (DoWhile b p : bs)
  | (Assign v (Phi e1 e2) : bs') <- b = Assign v e1 : unPhi (DoWhile (bs' ++ [Assign v e2]) p : bs)
  | otherwise = DoWhile (unPhi b) p : unPhi bs
unPhi (b : bs) = b : unPhi bs

------------------------------------------------------------------------
-- Part IV

makeSSA :: Function -> Function
makeSSA 
  = undefined


------------------------------------------------------------------------
-- Predefined functions for displaying functions and blocks...

opNames
  = [(Add, "+"), (Mul, "*"), (Eq, "=="), (Gtr, ">")]

precTable
  = [(Add, 1), (Mul, 2), (Eq, 0), (Gtr, 0)]

prec op
  = lookUp op precTable

showArgs [] 
  = ""
showArgs as
  = foldr1 (\a s -> a ++ (", " ++ s)) as

showExp :: Int -> Exp -> String
showExp _ (Const n) 
  = show n
showExp _ (Var id) 
  = id
showExp n (Apply op' e e') 
  | n > n'    = "(" ++ s ++ ")"
  | otherwise = s
  where 
    n' = prec op'
    s = showExp n' e ++ " " ++ fromJust (lookup op' opNames ) ++ " " ++ 
        showExp n' e'
showExp _ (Phi e e')
  = "PHI(" ++ showArgs (map (showExp 0) [e, e']) ++ ")"

showLine s n k
  =  putStrLn (show n ++ ": " ++ replicate (k + 2 - length (show n)) ' ' ++ s)

showBlock' b n
  = showBlock'' b n 2
  where
    showBlock'' :: Block -> Int -> Int -> IO Int
    showBlock'' [] n k
      = return n
    showBlock'' (s : b) n k
      = do n'  <- showStatement s n k
           n'' <- showBlock'' b n' k
           return n''
    showStatement (Assign id e) n k
      = do showLine (id ++ " = " ++ showExp 0 e) n k
           return (n + 1)
    showStatement (If p q []) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n' <- showBlock'' q (n + 1) (k + 2)
           showLine "}" n' k 
           return (n' + 1)
    showStatement (If p q r) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") n k
           n'  <- showBlock'' q (n + 1) (k + 2)
           showLine "} else {" n' k 
           n'' <- showBlock'' r (n' + 1) (k + 2)
           showLine "}" n'' k
           return (n'' + 1)
    showStatement (DoWhile b p) n k
      = do showLine "do {" n k
           n' <- showBlock'' b (n + 1) (k + 2)
           showLine ("} while " ++ showExp 9 p) n' k
           return (n' + 1)

showFun :: Function -> IO()
showFun (name, args, body)
  = do putStrLn ("1:  " ++ name ++ "(" ++ showArgs args ++ ") {")
       n <- showBlock' body 2
       showLine "}" n 0

showBlock ::  Block -> IO()
showBlock b
  = do n <- showBlock' b 1
       return ()

------------------------------------------------------------------------
-- Example state and expressions for testing...

s1 :: State
s1 = [("x", 7), ("y", 8)]

e1, e2, e3, e4, e5 :: Exp
e1 = Var "x"
e2 = Apply Mul (Apply Add (Var "x") (Const 1)) (Var "y")
e3 = Phi (Const 2) (Const 2)
e4 = Apply Add (Const 0) (Var "x")
e5 = Apply Add (Var "a") (Var "x")

------------------------------------------------------------------------
-- Example functions...

-- Figure 1...
example :: Function
example 
  = ("example",["x"],[Assign "a" (Const 1),Assign "b" (Apply Add (Var "x")
    (Const 2)),Assign "c" (Const 3),If (Apply Eq (Var "x") (Const 10)) 
    [Assign "a" (Const 1),Assign "c" (Const 5)] [],Assign "d" 
    (Apply Add (Var "a") (Const 3)),Assign "e" (Apply Add (Var "d") (Var "b")),
    Assign "$return" (Apply Add (Var "e") (Var "c"))])
    
exampleSSA :: Function
exampleSSA
  = ("example",["x"],[Assign "a0" (Const 1),Assign "b0" (Apply Add (Var "x")
    (Const 2)),Assign "c0" (Const 3),If (Apply Eq (Var "x") (Const 10)) [Assign
    "a1" (Const 1),Assign "c1" (Const 5)] [],Assign "a2" (Phi (Var "a1") (Var
    "a0")),Assign "c2" (Phi (Var "c1") (Var "c0")),Assign "d0" (Apply Add (Var
    "a2") (Const 3)),Assign "e0" (Apply Add (Var "d0") (Var "b0")),
    Assign "$return" (Apply Add (Var "e0") (Var "c2"))])
    
exampleSSAPropagated :: Function
exampleSSAPropagated
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [] [],Assign "c2" (Phi (Const 5) (Const 3)),
    Assign "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" 
    (Apply Add (Var "e0") (Var "c2"))])

exampleOptimised :: Function
exampleOptimised 
  = ("example",["x"],[Assign "b0" (Apply Add (Var "x") (Const 2)),If (Apply Eq
    (Var "x") (Const 10)) [Assign "c2" (Const 5)] [Assign "c2" (Const 3)],Assign
    "e0" (Apply Add (Const 4) (Var "b0")),Assign "$return" (Apply Add (Var "e0")
    (Var "c2"))])
    

-- Figure 2 (there is no SSA version of this)...
fact :: Function
fact 
  = ("fact", 
     ["n"], 
     [If (Apply Eq (Var "n") (Const 0))
        [Assign "$return" (Const 1)]
        [Assign "prod" (Const 1),
         Assign "i" (Var "n"),
         DoWhile 
           [Assign "prod" (Apply Mul (Var "prod") (Var "i")),
            Assign "i" (Apply Add (Var "i") (Const (-1)))
           ] 
           (Apply Gtr (Var "i") (Const 0)),
         Assign "$return" (Var "prod")
        ]
     ]
    )


-- Summation loop, specialised loop for the case k=0...
loop :: Function
loop 
  = ("loop",["n"],[Assign "i" (Var "n"),Assign "k" (Const 0),Assign "sum"
    (Const 0),If (Apply Eq (Var "i") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum" (Apply Add (Var "sum") (Apply Mul (Apply Add 
    (Var "i") (Apply Mul (Const 2) (Var "k"))) (Apply Add (Apply Add (Var "i") 
    (Apply Mul (Const 2) (Var "k"))) (Const 1)))),Assign "i" (Apply Add 
    (Var "i") (Const (-1)))] (Apply Gtr (Var "i") (Const 0)),
    Assign "$return" (Var "sum")]])
    
loopSSA :: Function
loopSSA
  = ("loop",["n"],[Assign "i0" (Var "n"),Assign "k0" (Const 0),Assign "sum0"
    (Const 0),If (Apply Eq (Var "i0") (Const 0)) [Assign "$return" (Const 0)]
    [DoWhile [Assign "sum1" (Phi (Var "sum0") (Var "sum2")),Assign "i1" 
    (Phi (Var "i0") (Var "i2")),Assign "k1" (Apply Mul (Var "k0") (Const 2)),
    Assign "a0" (Apply Add (Var "i1") (Var "k1")),Assign "k2" (Apply Mul 
    (Var "k0") (Const 2)),Assign "b0" (Apply Add (Var "k2") (Const 1)),
    Assign "b1" (Apply Add (Var "i1") (Var "b0")),Assign "m0" (Apply Mul 
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2") 
    (Const 0)),Assign "$return" (Var "sum2")]])
    
loopSSAPropagated :: Function
loopSSAPropagated 
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [DoWhile [Assign "sum1" (Phi (Const 0) (Var
    "sum2")),Assign "i1" (Phi (Var "i0") (Var "i2")),Assign "a0" (Var "i1"),
    Assign "b1" (Apply Add (Var "i1") (Const 1)),Assign "m0" (Apply Mul 
    (Var "a0") (Var "b1")),Assign "sum2" (Apply Add (Var "sum1") (Var "m0")),
    Assign "i2" (Apply Add (Var "i1") (Const (-1)))] (Apply Gtr (Var "i2") 
    (Const 0)),Assign "$return" (Var "sum2")]])
 
loopOptimised :: Function
loopOptimised 
  = ("loop",["n"],[Assign "i0" (Var "n"),If (Apply Eq (Var "i0") (Const 0))
    [Assign "$return" (Const 0)] [Assign "sum1" (Const 0),Assign "i1" (Var
    "i0"),DoWhile [Assign "a0" (Var "i1"),Assign "b1" (Apply Add (Var "i1") 
    (Const 1)),Assign "m0" (Apply Mul (Var "a0") (Var "b1")),Assign "sum2" 
    (Apply Add (Var "sum1") (Var "m0")),Assign "i2" (Apply Add (Var "i1") 
    (Const (-1))),Assign "sum1" (Var "sum2"),Assign "i1" (Var "i2")] 
    (Apply Gtr (Var "i2") (Const 0)),Assign "$return" (Var "sum2")]])
    

-- Basic block (no conditionals or loops)...
basicBlock :: Function
basicBlock 
  = ("basicBlock",[],[Assign "x" (Const 1),Assign "y" (Const 2),Assign "x"
    (Apply Add (Var "x") (Var "y")),Assign "y" (Apply Mul (Var "x") (Const
    3)),Assign "$return" (Var "y")])
    
basicBlockSSA :: Function
basicBlockSSA 
  = ("basicBlock",[],[Assign "x0" (Const 1),Assign "y0" (Const 2),Assign "x1"
    (Apply Add (Var "x0") (Var "y0")),Assign "y1" (Apply Mul (Var "x1") (Const
    3)),Assign "$return" (Var "y1")])
    
basicBlockSSAPropagated :: Function
basicBlockSSAPropagated
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- (This is the same as above, as there were no phi functions.)
basicBlockOptimised :: Function
basicBlockOptimised
  = ("basicBlock", [], [Assign "$return" (Const 9)])

-- Computes the maximum of two integers; useful for testing unPhi...
max2 :: Function
max2 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m" (Var "x")]
    [Assign "m" (Var "y")],Assign "$return" (Var "m")])

max2SSA :: Function
max2SSA 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2SSAPropagated :: Function
max2SSAPropagated
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x")] [Assign "m1" (Var "y")],Assign "m2" (Phi (Var "m0") (Var "m1")),Assign
    "$return" (Var "m2")])

max2Optimised :: Function
max2Optimised 
  = ("max2",["x","y"],[If (Apply Gtr (Var "x") (Var "y")) [Assign "m0" (Var
    "x"),Assign "m2" (Var "m0")] [Assign "m1" (Var "y"),Assign "m2" (Var
    "m1")],Assign "$return" (Var "m2")])


