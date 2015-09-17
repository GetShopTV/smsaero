module Main where

import Servant.Docs
import Servant.Docs.Pandoc
import Data.Proxy
import SMSAero (SMSAeroAPI)

main :: IO ()
main = makeFilter (docs (Proxy :: Proxy SMSAeroAPI))
