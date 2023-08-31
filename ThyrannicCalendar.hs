module UESRPG.ThyrannicCalendar where

import Text.Printf
import AbLib.Data.Tuple
import Control.Monad

data ThyrYear = ThyrYear
   { epoch :: Int
   , yearOfEpoch :: Int
   } deriving (Eq, Ord)
type SeqYear = Int
data GregYear = CE Int | BCE Int deriving (Show)

instance Show ThyrYear where
   show (ThyrYear e y) = show e ++ "," ++ show y

instance Show ThyrDate where
   show (ThyrDate y w d) = show w ++ suffix w ++ " " ++ show d ++ " of " ++ show y
   

-- CALENDRICAL CONSTANTS
origin = ThyrDate (ThyrYear 1 1) 1 Dolgos
daysPerYear = 6*56
daysPer20Years = 6*(56*20+14)
daysPerEpoch = daysPer20Years*10-6
daysPerMonth = alignmentPeriod (orbitalPeriod arukma) (orbitalPeriod losit)

-- CELESTIAL BODY CONSTANTS
data Body = Body
   { orbitalPeriod :: Double
   , originAngle :: Double -- syndodic
   } deriving (Show)

sun = Body 340.16433 11.2854 -- sidereal, degrees forward from spring/summer boundary
arukma = Body 17.79459 76.0417 -- synodic, degrees forward from sun
losit = Body 48.28098 321.7148  -- synodic, degrees forward from sun

dayAngle :: Body -> Double
dayAngle b = 360 / (orbitalPeriod b)

-- takes two orbital periods and produces the time between the objects' alignment
alignmentPeriod :: Double -> Double -> Double
alignmentPeriod a b = a*b / (abs (a-b))

suffix :: Int -> String
suffix n | (n `mod` 100) `elem` [11,12,13] = "th"
suffix n = case n `mod` 10 of
   1 -> "st"
   2 -> "nd"
   3 -> "rd"
   _ -> "th"

new :: Int -> Int -> ThyrYear
new = ThyrYear



weeksPerYear :: ThyrYear -> Int
weeksPerYear d = 56 + if yearOfEpoch d == 200 then 13 else if yearOfEpoch d `mod` 20 == 0 then 14 else 0


thyrToSeq :: ThyrYear -> SeqYear
thyrToSeq d = 200*(epoch d) + (yearOfEpoch d) - 2000

seqToThyr :: SeqYear -> ThyrYear
seqToThyr y = let
   (d,m) = (y+1999) `divMod` 200
   in new d (m+1)

seqToGreg :: SeqYear -> GregYear
seqToGreg d = if d > 0 then CE d else BCE (1-d)

gregToSeq :: GregYear -> SeqYear
gregToSeq (CE d) = d
gregToSeq (BCE d) = 1-d

thyrToGreg :: ThyrYear -> GregYear
thyrToGreg = seqToGreg . thyrToSeq

gregToThyr :: GregYear -> ThyrYear
gregToThyr = seqToThyr . gregToSeq


data DayOfWeek = Dolgos | Naugos | Brogos | Thyrgos | Draxigos | Telugos
   deriving (Eq, Ord, Bounded, Enum, Show, Read)

data ThyrDate = ThyrDate
   { year :: ThyrYear
   , weekNo :: Int
   , dayOfWeek :: DayOfWeek
   } deriving (Eq, Ord)

data TemporalUnit = Day | Week | Month | Year | Epoch



instance Enum ThyrYear where
   toEnum = seqToThyr
   fromEnum = thyrToSeq

instance Enum ThyrDate where
   fromEnum (ThyrDate ty@(ThyrYear e y) w d) = let
      e' = e - 1
      (p',y') = (y-1) `divMod` 20
      w' = w - 1
      d' = fromEnum d
      in e' * daysPerEpoch + p' * daysPer20Years + y' * daysPerYear + w' * 6 + d'

   toEnum n = let
      (e,r1) = mfst (+1) (n `divMod` daysPerEpoch)
      (p,r2) = mfst (*20) (r1 `divMod` daysPer20Years)
      (y,r3) = mfst (+1) (r2 `divMod` daysPerYear)
      (w,r4) = mfst (+1) (r3 `divMod` 6)
      d = toEnum r4
      in ThyrDate (ThyrYear e (p+y)) w d

class Enum a => Date a where
   valid :: a -> Bool
   offset :: Int -> TemporalUnit -> a -> a
   diff :: TemporalUnit -> a -> a -> Int
   solarAngle :: a -> Double -- angle forward from summer equinox (degrees)
   
   season :: a -> String
   season = getSeasonFromAngle . solarAngle

instance Date ThyrYear where
   valid (ThyrYear e y) = e >= 1 && y >= 1 && y <= 200
   
   offset n Epoch y = offset (200*n) Year y
   offset n Year y = seqToThyr (n + thyrToSeq y)
   offset n u y = year $ offset n u (ThyrDate y 1 Dolgos)

   diff Epoch a b = diff Year a b `div` 200
   diff Year a b = abs (fromEnum b - fromEnum a)
   diff u a b = diff u (dateFromYear a) (dateFromYear b)
   
   solarAngle (ThyrYear e y) = let
      p = ((y-1) `div` 20) + 1
      shift = (336 * (y - 1) + 84 * (p - 1) + daysPerEpoch * (e - 1))
      in fixAngle (originAngle sun + dayAngle sun * fromIntegral shift)

instance Date ThyrDate where
   valid (ThyrDate y w d) = valid y && 1 <= w && w <= weeksPerYear y
   
   offset n Epoch d = offset (200*n) Year d
   offset n Year d = offset (daysPerYear * n) Day d
   offset n Month d = offset (round (fromIntegral n * daysPerMonth)) Day d
   offset n Week d = offset (6*n) Day d
   offset n Day d = toEnum (fromEnum d + n)
   
   diff Month a b = floor (fromIntegral (diff Day a b) / daysPerMonth)
   diff Week a b = diff Day a b `div` 6
   diff Day a b = abs (fromEnum a - fromEnum b)
   diff u a b = diff u (year a) (year b)
   
   solarAngle (ThyrDate y w d) = let
      yearStartAngle = solarAngle y
      extraDays = diff Day (dateFromYear y) (ThyrDate y w d)
      in yearStartAngle + dayAngle sun * fromIntegral extraDays
   

newDate :: Int -> Int -> Int -> DayOfWeek -> ThyrDate
newDate e y w d = ThyrDate (ThyrYear e y) w d

new2022 = newDate 20 22

dateFromYear :: ThyrYear -> ThyrDate
dateFromYear y = ThyrDate y 1 $ toEnum 0

getSeasonFromAngle :: Double -> String
getSeasonFromAngle = aux . fixAngle where
   aux a
      | a < 30 = "Early Summer"
      | a < 60 = "Mid Summer"
      | a < 90 = "Late Summer"
      | a < 120 = "Early Autumn"
      | a < 150 = "Mid Autumn"
      | a < 180 = "Late Autumn"
      | a < 210 = "Early Winter"
      | a < 240 = "Mid Winter"
      | a < 270 = "Late Winter"
      | a < 300 = "Early Spring"
      | a < 330 = "Mid Spring"
      | a < 360 = "Late Spring"

rmod :: RealFrac a => a -> a -> a
rmod x n = x - n * (fromIntegral $ floor (x/n))

fixAngle :: (RealFrac a, Ord a) => a -> a
fixAngle = (`rmod` 360)

fixAngle180 :: (RealFrac a, Ord a) => a -> a
fixAngle180 a = let a' = fixAngle a in if a' > 180 then a' - 360 else a'

moonPhase :: Double -> String
moonPhase a = aux (moonIllumination a) where
   moonPhaseMargin = 0.05
   direction = if fixAngle a < 180 then "Waxing " else "Waning "
   aux i
      | i < 0.0 + moonPhaseMargin = "New Moon"
      | i < 0.5 - moonPhaseMargin = direction ++ "Crescent"
      | i < 0.5 + moonPhaseMargin = direction ++ "Half-Moon"
      | i < 1.0 - moonPhaseMargin = direction ++ "Gibbous"
      | otherwise = "Full Moon"

moonIllumination :: Double -> Double
moonIllumination a = (1 - cos(a * pi/180))/2

moonAngle :: Body -> ThyrDate -> Double
moonAngle moon date = let
   daysPassed = fromIntegral $ diff Day origin date
   extraDays = rmod daysPassed (orbitalPeriod moon)
   in fixAngle (originAngle moon + extraDays * dayAngle moon)

info :: ThyrDate -> IO ()
info date = do
   printf "Date: %s\n" (show date)
   printf "Season: %s\n" (season date)
   let arukmaAngle = moonAngle arukma date
   let lositAngle  = moonAngle losit date
   printf "Arukma: %3.0f°; %3.0f%% illumination (%s)\n" arukmaAngle (100 * moonIllumination arukmaAngle) (moonPhase arukmaAngle)
   printf "Losit:  %3.0f°; %3.0f%% illumination (%s)\n" lositAngle  (100 * moonIllumination lositAngle)  (moonPhase lositAngle)
   let angleDiff = fixAngle180 (arukmaAngle - lositAngle)
   printf "Angle between moons: %3.0f° (%s" (abs angleDiff) (if angleDiff > 0 then "widening" else "narrowing")
   if interlunarEclipse date then printf "; Interlunar eclipse today)\n" else printf ")\n"

moonPic :: Int -> Double -> IO ()
moonPic h a = let
   lum = moonIllumination a
   waxing = fixAngle a < 180
   arr = [[isMoon (x,y) | x <- [-sf*h..sf*h]] | y <- [-h..h]]
   in forM_ arr $ \row -> do
      let spaces = length $ takeWhile not row
      putStr $ replicate spaces ' '
      let moonTotal = length $ takeWhile id $ dropWhile not row
      let moonVisible = round (lum * fromIntegral moonTotal)
      let cellsVisible = replicate moonVisible '#'
      let cellsShadow = replicate (moonTotal - moonVisible) ' '
      let cellsMoon = concat $ (if waxing then id else reverse) [cellsShadow, cellsVisible]
      putStrLn cellsMoon
   where
   sf = 2
   isMoon (x,y) = let
      x' = fromIntegral x / fromIntegral sf
      y' = fromIntegral y
      h' = fromIntegral h
      in sqrt (x' ^2 + y' ^2) <= h'

interlunarEclipse :: ThyrDate -> Bool
interlunarEclipse today = let
   arukmaAngle = moonAngle arukma today
   lositAngle = moonAngle losit today
   tomorrow = offset 1 Day today
   arukmaAngleTmrw = moonAngle arukma tomorrow
   lositAngleTmrw = moonAngle losit tomorrow
   todayDiff = fixAngle180 (arukmaAngle - lositAngle)
   tomorrowDiff = fixAngle180 (arukmaAngleTmrw - lositAngleTmrw)
   in todayDiff < 0 && tomorrowDiff > 0

solarEclipse :: Body -> ThyrDate -> Bool
solarEclipse moon today = let
   angle = fixAngle180 $ moonAngle moon today
   angle' = fixAngle180 $ moonAngle moon $ offset 1 Day today
   in angle < 0 && angle' > 0

fullMoon :: Body -> ThyrDate -> Bool
fullMoon moon today = let
   angle = fixAngle180 $ moonAngle moon today
   angle' = fixAngle180 $ moonAngle moon $ offset 1 Day today
   in angle > 0 && angle' < 0

nextEvent :: (ThyrDate -> Bool) -> ThyrDate -> ThyrDate
nextEvent isEvent today = if isEvent today then today else nextEvent isEvent (offset 1 Day today)

nextInterlunarEclipse = nextEvent interlunarEclipse
nextSolarEclipse = nextEvent (\date -> solarEclipse arukma date || solarEclipse losit date)
nextFullMoon = nextEvent (\date -> fullMoon arukma date || fullMoon losit date)

unitTests :: IO ()
unitTests = do
   print (fromEnum (newDate 1 1 1 Dolgos) == 0)
   print (fromEnum (newDate 1 1 1 Naugos) == 1)
   print (fromEnum (newDate 1 1 1 Brogos) == 2)
   print (fromEnum (newDate 1 1 1 Thyrgos) == 3)
   print (fromEnum (newDate 1 1 1 Draxigos) == 4)
   print (fromEnum (newDate 1 1 1 Telugos) == 5)
   print (fromEnum (newDate 1 1 2 Dolgos) == 6)
   testOneDiff (newDate 1 1 56 Telugos) (newDate 1 2 1 Dolgos)
   testOneDiff (newDate 1 2 56 Telugos) (newDate 1 3 1 Dolgos)
   testOneDiff (newDate 1 20 70 Telugos) (newDate 1 21 1 Dolgos)
   testOneDiff (newDate 1 21 56 Telugos) (newDate 1 22 1 Dolgos)
   testOneDiff (newDate 1 100 70 Telugos) (newDate 1 101 1 Dolgos)
   testOneDiff (newDate 1 200 69 Telugos) (newDate 2 1 1 Dolgos)
   testOneDiff (newDate 20 22 16 Brogos) (newDate 20 22 16 Thyrgos)
   print (toEnum 1299878 == ThyrDate (ThyrYear 20 22) 16 Brogos)

testOneDiff :: ThyrDate -> ThyrDate -> IO ()
testOneDiff a b = print (fromEnum a + 1 == fromEnum b)