import Ribbons
import Test.QuickCheck
import Data.Maybe (isNothing)
import Data.Set (fromList)

instance Arbitrary a => Arbitrary (Interval a) where
  arbitrary =
    sized $ \n ->
      do basis <- arbitrary
         Positive start <- arbitrary
         return $ Interval basis start (start + n)

instance Arbitrary a => Arbitrary (Ribbon a) where
  arbitrary =
    do origin <- arbitrary
       dest <- arbitrary
       return $ Ribbon origin dest

instance Arbitrary a => Arbitrary (RibbonSet a) where
  arbitrary =
    do xs <- vectorOf 5 arbitrary
       return $ RibbonSet xs

prop_intersect_and :: Interval Int -> Interval Int -> Int -> Bool
prop_intersect_and intA intB p =
  let anded = (includes p intA) && (includes p intB) && (sameBasis intA intB)
      intersection = intersect intA intB
  in case intersection of
    Nothing -> not anded
    Just intSet -> (includes p intSet) == anded

prop_diff_and_not :: Interval Int -> Interval Int -> Int -> Bool
prop_diff_and_not intA intB p =
  let anded = (includes p intA) && ((not $ includes p intB) || (not $ sameBasis intA intB))
      diff = diff2 intA intB
  in anded == (or $ map (includes p) diff)

prop_normalize_intersection :: Interval Int -> Interval Int -> Bool
prop_normalize_intersection intA intB =
  let touches = touching intA intB
      IntervalSet normalized = normalize $ IntervalSet [intA, intB]
  in if touches then (length normalized) == 1 else (length normalized) == 2

prop_partial_inverse :: RibbonSet Int -> Int -> Bool
prop_partial_inverse ribbonSet p =
  let outputs = follow p ribbonSet
      reverses = outputs >>= (\p -> follow p (invert ribbonSet))
  in case reverses of
    [] -> not . and $ map (includes p . domainR) (ribbons ribbonSet)
    xs -> elem p xs

prop_compose_chain :: RibbonSet Int -> RibbonSet Int -> Int -> Bool
prop_compose_chain rsA rsB p =
  let shortOutput = follow p rsA
      longOutput = shortOutput >>= (\p -> follow p rsB)
      composedOutput = follow p (rsA <> rsB)
  in (fromList composedOutput) == (fromList longOutput)

main :: IO ()
main =
  do quickCheck prop_intersect_and
     quickCheck prop_diff_and_not
     quickCheck prop_normalize_intersection
     quickCheck prop_partial_inverse
