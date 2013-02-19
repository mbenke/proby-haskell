import Reg
import RegExtra

import Data.Functor
import Data.List(sort,group,nub)
import qualified Data.Set as Set

many1 r = r :> Many r

identifier = letter :> Many (letter :| digit)
main = findall identifier <$> getContents  >>= print -- . uniq
-- uniq = map head . group . sort
-- uniq = nub . sort
uniq :: Ord a => [a] -> [a]
uniq = Set.toAscList . Set.fromList