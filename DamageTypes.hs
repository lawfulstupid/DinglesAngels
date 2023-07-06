
module UESRPG.DamageTypes where

import Data.List
import AbLib.Data.List
import Control.Monad

data Armor = AR Int | NT Int
   deriving (Show, Eq)

getSoak :: Armor -> Int
getSoak (AR x) = x
getSoak (NT x) = x

getAr :: Armor -> Int
getAr (AR x) = x
getAr (NT _) = 0

data Scenario = Scenario
   { armor :: Armor
   , strengthBonus :: Int
   , baseDamage :: Int
   , degreesOfSuccess :: Int
   } deriving (Show)

--------------------------------------------------------------------------------

data DamageType = DamageType 
   { name :: String
   , resolve :: Scenario -> Int }
   
instance Eq DamageType where
   dt1 == dt2 = name dt1 == name dt2

instance Ord DamageType where
   compare dt1 dt2 = compare (name dt1) (name dt2)

instance Show DamageType where
   show = name

slashing :: DamageType
slashing = DamageType "Slashing" res where
   res x = max 0 $ case armor x of
      NT a -> baseDamage x + strengthBonus x - a
      AR a -> baseDamage x - a

crushing :: DamageType
crushing = DamageType "Crushing" res where
   res x = max 0 $ case armor x of
      NT a -> baseDamage x - getSoak (armor x)
      AR a -> baseDamage x - getEffectiveArmor a (strengthBonus x)
   getEffectiveArmor :: Int -> Int -> Int
   getEffectiveArmor ar sb = max 0 (ar - sb)

splitting :: DamageType
splitting = DamageType "Splitting" res where
   res x = let
      dmgToFace = max 0 (baseDamage x - getSoak (armor x))
      in dmgToFace + if dmgToFace > 0 then strengthBonus x else 0

splittingAlt :: DamageType
splittingAlt = DamageType "Splitting Alternative" res where
   res x = let
      dmgToFace = max 0 (baseDamage x - getSoak (armor x))
      in dmgToFace + if dmgToFace > 1 then strengthBonus x else 0

splittingV3 :: DamageType
splittingV3 = DamageType "Splitting V3" res where
   res x = let
      bonusDmg = if getAr (armor x) < degreesOfSuccess x then strengthBonus x else 0
      in max 0 (baseDamage x + bonusDmg - getSoak (armor x))

gate :: Int -> Int -> Int
gate ar dmg = if dmg > ar then dmg else 0

gatedSlashing :: DamageType
gatedSlashing = DamageType "Gated Slashing" res where
   res x = max 0 $ case armor x of
      NT a -> baseDamage x + strengthBonus x - a
      AR a -> gate a (baseDamage x)

gatedCrushing :: DamageType
gatedCrushing = DamageType "Gated Crushing" res where
   res x = max 0 $ case armor x of
      NT a -> baseDamage x - getSoak (armor x)
      AR a -> gate (getEffectiveArmor a (strengthBonus x)) (baseDamage x)
   getEffectiveArmor :: Int -> Int -> Int
   getEffectiveArmor ar sb = max 0 (ar - sb)

gatedSplitting :: DamageType
gatedSplitting = DamageType "Gated Splitting" res where
   res x = case armor x of 
      NT a -> let
         dmgToFace = max 0 (baseDamage x - a)
         in dmgToFace + if dmgToFace > 0 then strengthBonus x else 0
      AR a -> let
         dmgToFace = max 0 $ gate a $ baseDamage x
         in dmgToFace + if dmgToFace > 0 then strengthBonus x else 0

--------------------------------------------------------------------------------

armorRange :: [Armor]
armorRange = reverse (map NT [0..10]) ++ map AR [1..10]

damageRolls :: [Int]
damageRolls = do
   dieSize <- [6,8..12]
   roll <- [1..dieSize]
   flat <- [0..3]
   pure (roll + flat)

scenarios :: [Armor] -> [Scenario]
scenarios armors = do
   armor <- armors
   str <- [3..6]
   base <- damageRolls
   combatStyle <- [1..4]
   let maxDos = min 10 (str + combatStyle)
   dos <- 1:[1..maxDos]
   pure $ Scenario armor str base dos

bestDamageType :: [DamageType] -> Scenario -> [DamageType]
bestDamageType dts x = map fst
   $ head
   $ groupOn snd
   $ sortOn (negate.snd)
   $ zip dts
   $ map (flip resolve x) dts

results :: [DamageType] -> [([DamageType], Int)]
results dt = freq $ map (bestDamageType dt) (scenarios armorRange)

mean :: Real a => [a] -> Double
mean xs' = let
   xs = map toRational xs'
   in fromRational (sum xs / toRational (length xs))

-- how much damage dealt relative to original damage roll?
relativeDmg :: DamageType -> Scenario -> Double
relativeDmg dt x = let
   pureDmg = fromIntegral $ resolve dt x
   baseDmg = fromIntegral $ baseDamage x
   in pureDmg / baseDmg

graph :: DamageType -> IO ()
graph dt = mapM_ (putStrLn.show) $ map meanDamage armorRange where
   meanDamage :: Armor -> Double
   meanDamage ar = mean
      $ [relativeDmg dt x | x <- scenarios [ar] ]
