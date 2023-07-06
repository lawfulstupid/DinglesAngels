module UESRPG.Equipment where

import Data.Ratio
import System.Random

data Hand = TwoHanded | OneHanded | Versatile
type RollableTable = [RollableItem]

data RollableItem = RollableItem 
   { name :: String
   , price :: Rational }

instance Show RollableItem where
   show i = name i ++ " (" ++ (show $ fromRational $ price i) ++ ")"

weight :: RollableItem -> Double
weight i = fromRational (10000 / (price i))

roll :: RollableTable -> IO RollableItem
roll t = do
   let max = sum $ map weight t
   r <- randomRIO (0,max)
   pure $ aux t r
   where
   aux :: RollableTable -> Double -> RollableItem
   aux (i:t) x = if weight i > x then i else aux t (x - weight i)

rollCombo :: [RollableTable] -> IO RollableItem
rollCombo [] = pure $ RollableItem "\8" 1
rollCombo (t:ts) = do
   a <- roll t
   b <- rollCombo ts
   pure $ RollableItem (name a ++ " " ++ name b) (price a * price b)

reroll :: Rational -> IO RollableItem -> IO RollableItem
reroll limit ii = do
   i <- ii
   if price i > limit then reroll limit ii else pure i

rollMelee :: Rational -> IO RollableItem
rollMelee limit = reroll limit $ rollCombo [quality, meleeMaterials, meleeWeapons]


quality :: RollableTable
quality =
   [ RollableItem "Inferior" 0.5
   , RollableItem "Common" 1
   , RollableItem "Superior" 3
   ]


meleeMaterials :: RollableTable
meleeMaterials = 
   [ RollableItem "Wood" 0.5
   , RollableItem "Bone" 0.5
   , RollableItem "Chitin" 0.8
   , RollableItem "Iron" 0.8
   , RollableItem "Silver" 1.3
   , RollableItem "Steel" 1
   , RollableItem "Dwemer" 4
   , RollableItem "Moonstone" 5
   , RollableItem "Orichalcum" 3
   , RollableItem "Adamantium" 8
   , RollableItem "Malachite" 7
   , RollableItem "Stahlrim" 12
   , RollableItem "Daedric" 15
   , RollableItem "Ebony" 10
   , RollableItem "Dragonbone" 30
   ]

meleeWeapons :: RollableTable
meleeWeapons = 
   [ RollableItem "Great Sword" 300
   , RollableItem "Longsword" 175
   , RollableItem "Broadsword" 100
   , RollableItem "Sabre" 125
   , RollableItem "Shortsword" 75
   , RollableItem "Dagger" 45
   , RollableItem "Great Axe" 250
   , RollableItem "Battle Axe" 125
   , RollableItem "War Axe" 100
   , RollableItem "Hand Axe" 40
   , RollableItem "Great Flail" 300
   , RollableItem "Maul" 250
   , RollableItem "Warhammer" 175
   , RollableItem "Mace" 120
   , RollableItem "Flail" 180
   , RollableItem "Halberd" 175
   , RollableItem "Pike" 80
   , RollableItem "Lance" 140
   , RollableItem "Spear" 40
   , RollableItem "Quarterstaff" 35
   , RollableItem "Javelin" 40
   , RollableItem "Parrying Dagger" 50
   , RollableItem "Dai-Katana" 325
   , RollableItem "Tanto" 65
   , RollableItem "Katana" 200
   , RollableItem "Wakizashi" 90
   , RollableItem "Trident" 75
   , RollableItem "Rapier" 105
   , RollableItem "Scimitar" 135
   , RollableItem "Bola" 30
   , RollableItem "Frying Pan" 10
   , RollableItem "Punch Dagger" 55
   , RollableItem "Cestus" 45
   , RollableItem "Hook Sword" 85
   , RollableItem "Bill Hook" 150
   ]


rangedWeapons :: RollableTable
rangedWeapons =
   [ RollableItem "Arbalest" 700
   , RollableItem "Crossbow" 500
   , RollableItem "Longbow" 200
   , RollableItem "Shortbow" 100
   , RollableItem "Sling" 15
   ]


ammoMaterials :: RollableTable
ammoMaterials = 
   [ RollableItem "Chitin" 16
   , RollableItem "Iron" 16
   , RollableItem "Silver" 20
   , RollableItem "Steel" 20
   , RollableItem "Dwemer" 90
   , RollableItem "Moonstone" 100
   , RollableItem "Orichalcum" 80
   , RollableItem "Adamantium" 160
   , RollableItem "Malachite" 140
   , RollableItem "Stahlrim" 240
   , RollableItem "Daedric" 300
   , RollableItem "Ebony" 200
   , RollableItem "Dragonbone" 600
   ]

thrownWeapons :: RollableTable
thrownWeapons =
   [ RollableItem "Dagger" 45
   , RollableItem "Hand Axe" 40
   , RollableItem "Javelin" 40
   , RollableItem "Throwing Star" 10
   , RollableItem "Throwing Dart" 10
   ]


