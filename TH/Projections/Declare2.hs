{-# LANGUAGE TemplateHaskell #-}
module Declare2 where
import Language.Haskell.TH

import Build2

$(build_p1)
