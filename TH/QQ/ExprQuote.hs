-- Based on http://www.haskell.org/haskellwiki/Quasiquotation
module ExprQuote where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Expr

expr  :: QuasiQuoter
expr  =  QuasiQuoter { quoteExp = quoteExprExp }

quoteExprExp s = do
  pos <- getPosition
  exp <- parseExp pos s
  dataToExpQ (const Nothing) exp
  
getPosition = fmap transPos TH.location where
  transPos loc = (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
                 
