{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Lib
    ( startApp
    ) where

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.TH
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import Models

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "user" :> "isaac" :> Get '[JSON] User
          :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User
          :<|> "user" :> QueryParams "sports" String :> Header "X-Origin" String :> Get '[JSON] User

startApp :: IO ()
startApp = withStdoutLogger $ \logger ->
    do let settings = setPort 1234 $ setLogger logger defaultSettings
       runSettings settings app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = return users
    :<|> return isaac
    :<|> addUser
    :<|> bySports

bySports :: [String] -> Maybe String -> Handler User
bySports (x:xs) origin = return isaac

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        , User 3 "Stephen" "Hawking"
        ]

isaac :: User
isaac =  User 1 "Isaac" "Newton"

addUser :: User -> Handler User
addUser = return
