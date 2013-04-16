{-# LANGUAGE TemplateHaskell #-}
module Declare3 where
import Language.Haskell.TH

import Build3

$(build_ps)
