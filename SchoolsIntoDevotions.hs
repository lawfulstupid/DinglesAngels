module UESRPG.SchoolsInDevotions where

import AbLib.Data.List hiding (Side(..))
import Data.List
import Control.Monad

data School = A | C | D | I | M | R deriving (Eq, Ord, Enum, Bounded, Show)
type Options a = [a]
type Sequence a = [a]
type Set a = [a]

schools :: Sequence School
schools = [A ..]

type Pair = Set School
pairs :: Options Pair
pairs = choose 2 schools

type Triple = Set School
triples :: Options Triple
triples = choose 3 schools

groupsOf3 :: Set (Set Triple)
groupsOf3 = filter noTwoInCommon'' $ subsequences triples where
   noTwoInCommon'' :: Set Triple -> Bool
   noTwoInCommon'' [] = True
   noTwoInCommon'' (x:xs) = noTwoInCommon' x xs && noTwoInCommon'' xs

   noTwoInCommon' :: Triple -> Set Triple -> Bool
   noTwoInCommon' t ts = all (noTwoInCommon t) ts

   noTwoInCommon :: Triple -> Triple -> Bool
   noTwoInCommon x y = length (intersect x y) <= 1

toPair :: Triple -> Set Pair
toPair [x,y,z] = map sort [[x,y],[x,z],[y,z]]

toPairs :: Set Triple -> Set Pair
toPairs ts = ts >>= toPair

correspondingPairs :: Set Triple -> Set Pair
correspondingPairs ts = pairs \\ toPairs ts

type Domain = Set School
fills :: Set (Set Domain)
fills = map (\g -> g ++ correspondingPairs g) groupsOf3

gods :: Set (Set Domain)
gods = filter pred fills where
   pred :: Set Domain -> Bool
   pred ds = length ds > 7 -- 7 is too few gods
      && elem [I,M] ds     -- one of the gods must have domain [Illusion, Mysticism] (EXAMPLE)

summary = freq $ map length gods



type God = Domain
melednia = sort [M,A,C]
riald = sort [M,I]
arngorn = sort [A,M,D]
ashla = sort [R,I]
kseiel = sort [R,C]
zethkar = sort [I,A,D]
etorius = sort [C,D]
-- draklaz = sort [A,D,R]
-- Cedelas = 


-- A ===
-- C ===
-- D ===
-- I ===
-- M ===
-- R ==

godsFinal = [melednia, riald, ashla, arngorn, kseiel, zethkar, etorius]

unsatisfiableDomains :: [God] -> [Domain]
unsatisfiableDomains gs = filter (not . maxPick2) triples where
   -- dubs :: [Set Domain]
   dubs = [sort (a `union` b) | a <- gs, b <- gs]
   
   maxPick2 :: Domain -> Bool
   maxPick2 dom = any (\choice -> dom `isSubsequenceOf` choice) dubs

