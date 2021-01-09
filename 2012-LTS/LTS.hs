import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp = (.) fromJust . lookup

states :: LTS -> [State]
states = nub . concatMap ((\(s1, s2) -> [s1, s2]) . fst)

transitions :: State -> LTS -> [Transition]
transitions s = filter ((==s) . fst . fst)

alphabet :: LTS -> Alphabet
alphabet = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions = nub . ac
  where
    ac (Prefix i p) = i : ac p
    ac (Choice ps)  = ps >>= ac
    ac _            = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts [] _ = True
accepts ids pds@(p : _) = ac ids p
  where
    ac :: [Id] -> ProcessDef -> Bool
    ac [] _              = True
    ac _ (_, STOP)       = False
    ac is (s, Ref t)     = ac is (s, lookUp t pds)
    ac (i : is) (s, Prefix t t')
      | i == t           = ac is (s, t')
      | otherwise = False
    ac is (s, Choice ts) = any (ac is) (zip (repeat s) ts)

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), is) ((s', t'), it) as at m
  | is == it = [((ss, tt), is)]
  | elem is at && elem it as = []
  | elem it as = [((ss, ts), is)]
  | elem is at = [((ss, st), it)]
  | otherwise = [((ss, ts), is), ((ss, st), it)]
  where
    p!q = lookUp (p, q) m
    [ss, tt, st, ts] = [s!s', t!t', s!t', t!s']

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit s vs
      | elem s vs = []
      | otherwise = trs ++ concatMap (\((f, t), a) -> visit t (s : vs)) trs
        where
          trs = transitions s ts

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose l1 l2 = (nub . pruneTransitions . filter (flip notElem ["$", "$'"] . snd)) (concatMap ct ts)
  where
    scp = [(s, t) | s <- states l1, t <- states l2]
    tcp (s, t) = [(st, tt) | st <- ((s, s), "$") : transitions s l1,
                             tt <- ((t, t), "$'") : transitions t l2]
    ts = concatMap tcp scp
    ct (s, t) = composeTransitions s t ("$" : alphabet l1) ("$'" : alphabet l2) (zip scp [0..])

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

