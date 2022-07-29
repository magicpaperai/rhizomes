{-# LANGUAGE GADTs, StandaloneDeriving #-}

module Main where
import Data.List (partition)

data Interval a where
  Interval :: Eq a => a -> Integer -> Integer -> Interval a
deriving instance Show a => Show (Interval a)
deriving instance Eq a => Eq (Interval a)

data Ribbon a where
  Ribbon :: Eq a => Interval a -> Interval a -> Ribbon a
deriving instance Show a => Show (Ribbon a)

data RibbonSet a where
  RibbonSet :: Eq a => [Ribbon a] -> RibbonSet a
deriving instance Show a => Show (RibbonSet a)

intersect :: Interval a -> Interval a -> [Interval a]
intersect (Interval basis start end) (Interval otherBasis cropStart cropEnd)
  | basis /= otherBasis = []
  | otherwise = if isEmpty then [] else [implied]
    where implied = Interval basis (max cropStart start) (min cropEnd end)
          isEmpty = impliedEnd <= impliedStart
          Interval _ impliedStart impliedEnd = implied

sameBasis :: Interval a -> Interval a -> Bool
sameBasis (Interval a _ _) (Interval b _ _) = a == b

diff :: Interval a -> Interval a -> [Interval a]
diff interval@(Interval basis start end) crop@(Interval _ minusStart minusEnd)
  | not $ sameBasis interval crop = [interval]
  | otherwise = case (intersect interval crop, minusStart > start, minusEnd < end) of
      ([], _, _) -> [interval]
      (_, True, True) -> [Interval basis start minusStart, Interval basis minusEnd end]
      (_, True, False) -> [Interval basis start minusStart]
      (_, False, True) -> [Interval basis minusEnd end]
      (_, False, False) -> []

diffN :: Interval a -> [Interval a] -> [Interval a]
diffN interval [] = [interval]
diffN interval (crop:rest) =
  do cropped <- diff interval crop
     diffN cropped rest

invert :: Ribbon a -> Ribbon a
invert (Ribbon x y) = Ribbon y x

translate :: Ribbon a -> Integer -> Integer
translate (Ribbon (Interval _ originStart _) (Interval _ destStart _)) point
  = point - originStart + destStart

translateI :: Ribbon a -> Interval a -> Interval a
translateI ribbon@(Ribbon origin (Interval dest _ _)) interval@(Interval _ start end)
  | not $ sameBasis origin interval = interval
  | otherwise = Interval dest (translate ribbon start) (translate ribbon end)

over :: Ribbon a -> Interval a -> [Interval a]
over ribbon@(Ribbon origin dest) interval
  | not $ sameBasis origin interval = [interval]
  | otherwise = terminal ++ continued
  where translated = translateI ribbon interval
        continued = intersect translated dest
        precontinued = map (translateI (invert ribbon)) continued
        terminal = diffN interval precontinued

compose :: Ribbon a -> Ribbon a -> RibbonSet a
compose a@(Ribbon _ destA) b@(Ribbon _ destB)
  = RibbonSet $ terminalRibbons ++ continuedRibbons
  where translated = over b destA
        (continued, terminal) = partition (sameBasis destB) translated
        reverseB = translateI (invert b)
        reverseA = translateI (invert a)
        continuedRibbons = map (\i -> Ribbon (reverseA . reverseB $ i) i) continued
        terminalRibbons =  map (\i -> Ribbon (reverseA i) i) terminal

instance Semigroup (RibbonSet a) where
  (RibbonSet []) <> xs = xs
  xs <> (RibbonSet []) = xs
  (RibbonSet xs) <> (RibbonSet ys) = RibbonSet $
    do x <- xs
       y <- ys
       (RibbonSet xs) <- return $ compose x y
       xs

instance Eq a => Monoid (RibbonSet a) where
  mempty = RibbonSet []

main :: IO ()
main =
  do
    putStrLn . show $ a
    putStrLn . show $ b
    putStrLn . show $ (RibbonSet [a]) <> (RibbonSet [b])
  where a = Ribbon (Interval "a" 10 50) (Interval "b" 100 140)
        b = Ribbon (Interval "b" 130 150) (Interval "c" 200 220)
