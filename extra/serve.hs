{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Control.Applicative             (empty, pure)
import Control.Category.Unicode        ((∘))
import Data.Bool                       (Bool(True))
import Data.Function                   (($), const)
import Data.List.Unicode               ((∈))
import Network.HTTP.Types.Status       (ok200)
import Network.Wai                     (responseFile)
import Network.Wai.Handler.Warp        (run)
import Network.Wai.Middleware.HttpAuth (authIsProtected, basicAuth)
import System.IO                       (IO)

main ∷ IO ()
main
  = run 3001
  ∘ basicAuth
    authenticate
    "Diagrama" { authIsProtected = isProtected }
  ∘ const
  ∘ pure
  $ responseFile ok200 empty "./graph.svg" empty
  where
    isProtected _
      = pure True

    authenticate username password
      = pure
      $ credentials ∈ authorization
      where
        credentials
          = (username, password)

        authorization
          = [ ("doc", "xyzzy")
            ]
