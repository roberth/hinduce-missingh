module Data.List.HIUtils (
  -- * Sorting-like
  uniqSort, aggregate, aggregateBy, aggregateAL,
  
  -- * Numeric lists operations
  average , averageI, relFreq, relFreqAL
  ) where
import Data.List
import Data.Ord
import Control.Arrow

-- TODO more checking

-- | Sort a list and leave out duplicates. Like @nub . sort@ but faster.
uniqSort :: (Ord a) => [a] -> [a]
uniqSort = map head . group . sort

-- TODO size constraint (nub complexity is :( )
prop_uniqSortIsNubSort :: [Int] -> Bool
prop_uniqSortIsNubSort a = uniqSort a == nub (sort a)

-- | Calculate the arithmetic mean. May not be numerically robust for some types and lists. Double and Rational should be fine most of the time.
average :: (Fractional a) => [a] -> a
average xs = (sum xs) / (genericLength xs)

-- | Shortcut for @average . map fromIntegral@. Check numerical robustness, see @average@ above.
averageI :: (Integral i, Fractional a) => [i] -> a
averageI = average . map fromIntegral

-- | Turn a list of integer frequencies into relative frequencies that sum up to 1. Frequencies should be nonnegative.
relFreq :: (Integral i, Fractional f) => [i] -> [f]
relFreq items = map divide items  
        where divide nom = fromIntegral nom / s
              s = fromIntegral (sum items)

-- | Turn an association list of integer frequencies into on of relative frequencies that sum up to 1. Frequencies should be nonnegative.
relFreqAL :: (Integral i, Fractional f) => [(a, i)] -> [(a, f)]
relFreqAL items = map (second divide) items
        where divide nom = fromIntegral nom / s
              s = fromIntegral . sum . map snd $ items

-- | Sort, then group
aggregate :: (Ord a) => [a] -> [[a]]
aggregate = aggregateBy compare

-- | Sort, then group
aggregateBy :: (a -> a -> Ordering) -> [a] -> [[a]]
aggregateBy x = groupBy (\a b -> x a b == EQ) . sortBy x

-- | Aggregate an association list, such that keys become unique.
aggregateAL :: (Ord a) => [(a,b)] -> [(a,[b])]
aggregateAL = map (fst . head &&& map snd) . aggregateBy (comparing fst)

-- | Find the most frequently occurring element. TODO: rewrite for Eq
majority :: (Ord a) => [a] -> a
majority = head . maximumBy (comparing length) . aggregate
