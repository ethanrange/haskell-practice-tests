data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix f s = f == take (length f) s

removePrefix :: String -> String -> String
removePrefix = drop . length

suffixes :: [a] -> [[a]]
suffixes []         = []
suffixes x@(_ : xs) = x : suffixes xs

isSubstring :: String -> String -> Bool
isSubstring = (. suffixes) . any . isPrefix

findSubstrings :: String -> String -> [Int]
findSubstrings f s = [n | (n, b) <- zip [0..] (suffixes s), isPrefix f b]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node l) = l >>= (getIndices . snd)

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition a b = (pr, as, bs)
    where
        pr = map fst (takeWhile (uncurry (==)) (zip a b))
        as = drop (length pr) a
        bs = drop (length pr) b

partition' :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition' ta@(a : as) tb@(b : bs)
    | a == b = app a (partition' as bs)
    | otherwise = ([], ta, tb)
    where
        app e (x, y, z) = (e : x, y, z)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf n) = [n]
findSubstrings' _ (Leaf _)  = []
findSubstrings' s (Node l) = concatMap fs l
    where
        fs (a, t)
            | isPrefix s a = getIndices t
            | isPrefix a s = findSubstrings' (removePrefix a s) t
            | otherwise = []



------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert 
  = undefined

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

