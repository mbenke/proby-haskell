{-# LANGUAGE TypeFamilies,  MultiParamTypeClasses,
             TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld -- must be after mkYesod

getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|Hello world!|]

main = warpEnv HelloWorld

