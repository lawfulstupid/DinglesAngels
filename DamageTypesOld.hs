
module UESRPG.DamageTypes where

import AbLib.Data.List

data Armor = AR Int | NT Int
   deriving (Show, Eq)

data Scenario = Scenario
   { armor :: Armor
   , strengthBonus :: Int
   , baseDamage :: Int
   , dos :: Int }
   deriving (Show)

getAr :: Armor -> Int
getAr (AR x) = x
getAr (NT _) = 0

getSoak :: Armor -> Int
getSoak (AR x) = x
getSoak (NT x) = x

getAltSoak :: Armor -> Int
getAltSoak (AR x) = x
getAltSoak (NT x) = floor $ log (fromIntegral x)

slashing :: Scenario -> Int
slashing x = baseDamage x + (if getAr (armor x) == 0 then strengthBonus x else 0)

crushing :: Scenario -> Int
crushing x = baseDamage x + minimum [strengthBonus x, getAr (armor x)]

splittingType = 2

splitting :: Scenario -> Int
splitting = case splittingType of
   2 -> splitting2
   3 -> splitting3
   4 -> splittingJoe
   5 -> splittingTest
   6 -> splittingTest2
-- splitting1 best only when AR = 0 and DoS = 0
-- splitting2 best 

-- Splitting V2 (+SB on dmg)
splitting2 :: Scenario -> Int
splitting2 x = let
   dmgToFace = baseDamage x - getSoak (armor x)
   in baseDamage x + (if dmgToFace > 0 then strengthBonus x else 0)

-- Splitting V3 (default DoS model)
splitting3 :: Scenario -> Int
splitting3 x = baseDamage x + (if dos x > getAr (armor x) then strengthBonus x else 0)

splittingJoe :: Scenario -> Int
splittingJoe x = let
   dmgToFace = baseDamage x - getSoak (armor x)
   pass = dos x > getAr (armor x) || dmgToFace > 0
   in baseDamage x + (if pass then min (strengthBonus x) (getAr (armor x)) else 0)

splittingTest :: Scenario -> Int
splittingTest x = let
   dmgToFace = baseDamage x - getSoak (armor x)
   strong = strengthBonus x < getSoak (armor x)
   in baseDamage x + (if dmgToFace > 0 || strong then strengthBonus x else 0)

splittingTest2 :: Scenario -> Int
splittingTest2 x = baseDamage x + (if dos x >= getSoak (armor x) then strengthBonus x else 0)

armors :: [Armor]
armors = reverse (map NT [1..10]) ++ map AR [0..10]

dosRange :: [Int]
dosRange = if splittingType `elem` [3,4,6] then [1..6] else [-1]

scenarios :: [Scenario]
scenarios = [Scenario armor str base dos | armor <- armors, str <- [3..7], base <- [1..12], dos <- dosRange]

data DamageType = Splitting | Slashing | Crushing
   deriving (Show, Eq)

dtf :: DamageType -> Scenario -> Int
dtf dt = case dt of
   Splitting -> splitting
   Slashing -> slashing
   Crushing -> crushing

bestDamageType :: Scenario -> [DamageType]
bestDamageType x = let
   slash = max 0 $ slashing x
   split = max 0 $ splitting x
   crush = max 0 $ crushing x
   top = maximum [slash, split, crush]
   in (if slash == top then [Slashing] else []) ++ (if split == top then [Splitting] else []) ++ (if crush == top then [Crushing] else [])

results :: [([DamageType], Int)]
results = freq $ map bestDamageType scenarios

best :: DamageType -> [Scenario]
best dt = filter (\x -> bestDamageType x == [dt]) scenarios

graph :: DamageType -> [Double]
graph dt = map meanDamage armors where
   meanDamage :: Armor -> Double
   meanDamage ar = mean
      $ map fromIntegral
      $ map (max 0)
      $ [dtf dt $ Scenario ar sb base dos | sb <- [3..7], base <- [1..12], dos <- dosRange ]

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)