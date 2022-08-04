{-# LANGUAGE GADTs, DeriveFunctor, BlockArguments #-}
module Ribbons where
import Data.List (sort, elemIndex, groupBy, find)
import Data.Maybe (fromMaybe, isNothing, maybeToList, catMaybes)

data Interval a = Interval a Int Int deriving Show
data IntervalSet a = IntervalSet [Interval a] deriving Show
data Ribbon a = Ribbon (Interval a) (Interval a) deriving Show
data RibbonSet a = RibbonSet [Ribbon a] deriving Show

instance Eq a => Eq (Interval a) where
  (Interval b1 s1 e1) == (Interval b2 s2 e2) = b1 == b2 && s1 == s2 && s1 == s2
instance Eq a => Ord (Interval a) where
  (Interval _ a _) <= (Interval _ b _) = a <= b

-- intersect two intervals, returning Nothing if no intersection
intersect :: Eq a => Interval a -> Interval a -> Maybe (Interval a)
intersect (Interval basis start end) (Interval otherBasis cropStart cropEnd)
  | basis /= otherBasis = Nothing
  | otherwise = if isEmpty then Nothing else Just implied
    where implied = Interval basis (max cropStart start) (min cropEnd end)
          Interval _ impliedStart impliedEnd = implied
          isEmpty = impliedEnd <= impliedStart

-- simple predicate that compares bases of two intervals
sameBasis :: Eq a => Interval a -> Interval a -> Bool
sameBasis (Interval a _ _) (Interval b _ _) = a == b

-- subtract one interval from another, result is 0 to 2 intervals
diff2 :: Eq a => Interval a -> Interval a -> [Interval a]
diff2 interval@(Interval basis start end) crop@(Interval _ minusStart minusEnd)
  | not $ sameBasis interval crop = [interval]
  | otherwise = case (intersect interval crop, minusStart > start, minusEnd < end) of
      (Nothing, _, _) -> [interval]
      (_, True, True) -> [Interval basis start minusStart, Interval basis minusEnd end]
      (_, True, False) -> [Interval basis start minusStart]
      (_, False, True) -> [Interval basis minusEnd end]
      (_, False, False) -> []

-- recursively subtract one IntervalSet from another
diff (IntervalSet []) _ = IntervalSet []
diff intervalSet (IntervalSet []) = intervalSet
diff (IntervalSet [interval]) (IntervalSet (crop:rest)) =
  diff (IntervalSet $ diff2 interval crop) (IntervalSet rest)
diff (IntervalSet (interval:rest)) crop =
  let (IntervalSet a) = diff (IntervalSet [interval]) crop
      (IntervalSet b) = diff (IntervalSet rest) crop
  in IntervalSet $ a ++ b

-- sort a list of intervals such that adjacent terms have the same basis and are ordered by interval start
-- this latter ordering is encoded by the implementation of Ord (Interval a)
sorted :: Eq a => [Interval a] -> [Interval a]
sorted xs =
  let bases = map (\(Interval basis _ _) -> basis) xs
      tagged = map (\(Interval b s e) -> (fromMaybe 0 $ elemIndex b bases, Interval b s e)) xs
  in map snd $ sort tagged

-- function defined specifically for use in the following normalize function
-- this is the reducer that will merge overlapping adjacent intervals
mergeInterval :: Eq a => [Interval a] -> Interval a -> [Interval a]
mergeInterval [] x = [x]
mergeInterval (x:xs) y
  | not $ isNothing (intersect x y) = (Interval basis (min a1 a2) (max b1 b2)):xs
      where (Interval basis a1 b1, Interval _ a2 b2) = (x, y)
mergeInterval xs y = y:xs

-- normalize an IntervalSet by merging overlapping intervals and sorting
normalize (IntervalSet xs) =
  let grouped = groupBy sameBasis $ sorted xs
      merged = grouped >>= (foldl mergeInterval [])
  in IntervalSet merged

-- translate turns a Ribbon into a function on points
-- its behavior is simplistic and unbounded by the origin interval of the Ribbon
translate :: Ribbon a -> Int -> Int
translate (Ribbon (Interval _ originStart _) (Interval _ destStart _)) point
  = point - originStart + destStart

-- translateI turns a Ribbon into a function on Intervals
-- the result is restricted to the domain and range of the ribbon
translateI :: Eq a => Ribbon a -> Interval a -> Maybe (Interval a)
translateI ribbon@(Ribbon origin (Interval dest _ _)) interval@(Interval _ start end)
  | not $ sameBasis origin interval = Nothing
  | otherwise = intersect extrapolate range
    where extrapolate = Interval dest (translate ribbon start) (translate ribbon end)
          Ribbon domain range = ribbon

-- invert the direction of a RibbonSet, effectively inverting the function
invert (RibbonSet ribbons) = RibbonSet $ map (\(Ribbon a b) -> Ribbon b a) ribbons

-- helper function to pull the ribbons out of a RibbonSet
ribbons :: RibbonSet a -> [Ribbon a]
ribbons (RibbonSet rs) = rs

-- the image of an IntervalSet over a RibbonSet is another IntervalSet having translated intervals over ribbons
image :: Eq a => RibbonSet a -> IntervalSet a -> IntervalSet a
image (RibbonSet ribbons) (IntervalSet intervals) =
  normalize $ IntervalSet $
    do ribbon <- ribbons
       interval <- intervals
       maybeToList $ translateI ribbon interval

-- the preimage is the inverse of the image, but the result of composing the two will be a subset of the input
-- limited to the domain of the RibbonSet
preimage ribbonSet = image (invert ribbonSet)

-- range defined for Ribbons and RibbonSets
rangeR :: Eq a => Ribbon a -> Interval a
rangeR (Ribbon _ dest) = dest
range :: Eq a => RibbonSet a -> IntervalSet a
range (RibbonSet ribbons) = normalize . IntervalSet $ map rangeR ribbons

-- domain defined for Ribbons and RibbonSets
domainR :: Eq a => Ribbon a -> Interval a
domainR (Ribbon origin _) = origin
domain :: Eq a => RibbonSet a -> IntervalSet a
domain (RibbonSet ribbons) = normalize . IntervalSet $ map domainR ribbons

-- the partial Ribbon over an Interval is the original Ribbon with its domain restricted
partialI :: Eq a => Interval a -> Ribbon a -> Maybe (Ribbon a)
partialI interval ribbon =
  let target = translateI ribbon interval
  in fmap (\dest -> Ribbon interval dest) target

-- the partial RibbonSet over an Interval consists of all Ribbons for that part of the domain
partial :: Eq a => RibbonSet a -> Interval a -> RibbonSet a
partial (RibbonSet ribbons) interval =
  let overlapping = filter (not . isNothing . intersect interval . domainR) ribbons
  in RibbonSet . catMaybes $ map (partialI interval) overlapping

-- prism is an extension of partial in which an interval is projected in two directions,
-- over a forward RibbonSet and a backward RibbonSet. Its effect is multiplicative.
prism :: Eq a => RibbonSet a -> RibbonSet a -> Interval a -> RibbonSet a
prism forward backward interval =
  let forwardRibbons = ribbons $ partial forward interval
      backwardRibbons = ribbons $ partial (invert backward) interval
  in RibbonSet $
      do (Ribbon _ dest) <- forwardRibbons
         (Ribbon _ origin) <- backwardRibbons
         return $ Ribbon origin dest

compose :: Eq a => RibbonSet a -> RibbonSet a -> RibbonSet a
compose f g =
  let long = image g $ range f
      prelong = preimage g $ long
      short = diff (range f) prelong
      preshort = preimage f short
      IntervalSet prelongs = prelong
      IntervalSet preshorts = preshort
      shorts = preshorts >>= (ribbons . partial f)
      longs = prelongs >>= (ribbons . prism g f)
  in RibbonSet $ shorts ++ longs

instance Eq a => Semigroup (RibbonSet a) where
  (<>) = compose

instance Eq a => Monoid (RibbonSet a) where
  mempty = RibbonSet []

includes :: Int -> Interval a -> Bool
includes point (Interval _ start end) = point >= start && point < end

followR :: Eq a => Int -> Ribbon a ->  Maybe Int
followR point ribbon@(Ribbon _ dest) =
  let translated = translate ribbon point
  in if includes translated dest
     then Just translated
     else Nothing

follow :: Eq a => Int -> RibbonSet a -> [Int]
follow point (RibbonSet ribbons) = catMaybes $ map (followR point) ribbons
