-- mainmodule TestTreeDict where
import Test.QuickCheck
import Control.Monad(liftM3)
import Data.List(sortBy, nubBy)
import Data.Function
import Prelude hiding(lookup)
import TreeDict

main = test
test = do
     quickCheck propLookupEmpty
     quickCheck propLookupInsert
     quickCheck propSort

propLookupEmpty :: Int -> Bool
propLookupEmpty x = lookup x (empty::Dict Int Int) == Nothing

propLookupInsert :: Int -> Int -> Int -> [(Int,Int)] -> Bool
propLookupInsert x y v xs = lookup x (insert y v d) == if x == y then Just v else lookup x d 
  where d = fromList xs

propSort :: [(Int,Int)] -> Bool
propSort xs = sortByFst (nubByFst xs) == (sortByFst . toList . fromList $ xs ) where 
 sortByFst = Data.List.sortBy (compare `on` fst)
 nubByFst = Data.List.nubBy ((==) `on` fst)
 on = Data.Function.on
