{-# LANGUAGE TemplateHaskell #-}
module Declare1 where
import Language.Haskell.TH

import Build1

$(build_p1)
