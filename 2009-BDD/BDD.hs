import Data.List
import Data.Maybe

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp = (.) fromJust . lookup

checkSat :: BDD -> Env -> Bool
checkSat (i, ns) e = cs (i, lookUp i ns)
  where
    cs :: BDDNode -> Bool
    cs (0, _)       = False 
    cs (1, _)       = True
    cs (_, (ix, l, r))
      | lookUp ix e = cs (r, lookUp r ns)
      | otherwise   = cs (l, lookUp l ns)

sat :: BDD -> [[(Index, Bool)]]
sat (i, ns) = st [] (i, lookUp i ns)
  where
    st :: Env -> BDDNode -> [Env]
    st _ (0, _) = []
    st e (1, _) = [e]
    st e (_, (ix, l, r))
      = st ((ix, True) : e) (r, lookUp r ns) ++ st ((ix, False) : e) (l, lookUp l ns) 

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim b))            = Prim (not b)
simplify (Or (Prim b1) (Prim b2))  = Prim (b1 || b2)
simplify (And (Prim b1) (Prim b2)) = Prim (b1 && b2)
simplify b                         = b

restrict :: BExp -> Index -> Bool -> BExp
restrict (Prim b) _ _  = Prim b
restrict (IdRef n) i b
  | n == i    = Prim b
  | otherwise = IdRef n
restrict (Not e) i b   = simplify (Not (restrict e i b))
restrict (Or l r) i b  = simplify (Or (restrict l i b) (restrict r i b))
restrict (And l r) i b = simplify (And (restrict l i b) (restrict r i b))

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD = buildBDD' 2

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: NodeId -> BExp -> [Index] -> BDD
buildBDD' _ (Prim n) []
  | n         = (1, [])
  | otherwise = (0, [])
buildBDD' id ex (i : is) = (id, (id, (i, li, ri)) : ls ++ rs)
  where 
    (li, ls) = buildBDD' (2 * id) (restrict ex i False) is
    (ri, rs) = buildBDD' (2 * id + 1) (restrict ex i True) is

------------------------------------------------------
-- PART IV

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e i = (2, [n | n@(_, (_, li, ri)) <- ns, li /= ri])
  where
    (_, ns) = buildROBDD' 2 e i

buildROBDD' :: NodeId -> BExp -> [Index] -> BDD
buildROBDD' _ (Prim n) []
  | n         = (1, [])
  | otherwise = (0, [])
buildROBDD' id ex (i : is) = (id, (id, (i, lf, rf)) : ls ++ rs)
  where 
    (li, ls) = buildROBDD' (2 * id) (restrict ex i False) is
    (ri, rs) = buildROBDD' (2 * id + 1) (restrict ex i True) is
    lf = last $ li : [lli | (u, (_, lli, lri)) <- ls, lli == lri]
    rf = last $ ri : [rli | (u, (_, rli, rri)) <- rs, rli == rri]

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])


