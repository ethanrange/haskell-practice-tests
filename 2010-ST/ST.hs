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
partition s1@(c : cs) s2@(w : ws)
  | c == w = (c : cs', ws', zs')
  | otherwise = ([], s1, s2)
  where
    (cs', ws', zs') = partition cs ws
partition s1 s2 = ([], s1, s2)

findSubstrings'' :: String -> SuffixTree -> [Int]
findSubstrings'' "" (Leaf n) = [n]
findSubstrings'' _ (Leaf _)  = []
findSubstrings'' s (Node l) = l >>= fs
    where
        fs (a, t)
            | isPrefix s a = getIndices t
            | isPrefix a s = findSubstrings'' (removePrefix a s) t
            | otherwise = []

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf n) = [n]
findSubstrings' _ (Leaf _)  = []
findSubstrings' s (Node l) = l >>= fs
  where
    fs (a, t) = case partition s a of
      (_, "", _) -> getIndices t
      (_, m, "") -> findSubstrings' m t
      _          -> []

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node ls)
  | (not . and) [null m | (l, _) <- ls, let (m, _, _) = partition s l] = Node (map inspect ls)
  | otherwise = Node ((s, Leaf n) : ls)
  where
    inspect :: (String, SuffixTree) -> (String, SuffixTree)
    inspect (a, t) = case partition s a of
      ("", _, _)  -> (a, t)
      (p, r, "")  -> (a, insert (r, n) t)
      (p, r1, r2) -> (p, Node [(r1, Leaf n), (r2, t)])

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring (Node x) = (snd . maximum . concat) ([(0, "")] : [longest (s, Node t) | (s, Node t) <- x])
  where
    longest (s, Node ls) = (length s, s) : concat [longest (s ++ st, Node t) | (st, Node t) <- ls]
longestRepeatedSubstring _        = ""

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

