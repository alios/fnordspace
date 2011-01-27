{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Maybe
import Data.Map (findWithDefault)
import Network.URI
import Network.HTTP
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Browser


fnordspaceOAuthAPIKey = "mJKuzrfsLKrccB4SuHRnfw"
--app = 


accUrl    = fromJust . parseURL $ "https://api.twitter.com/oauth/authorize"
srvUrl    = fromJust . parseURL $ "https://service/path/to/resource/"


app       = Application "consumerKey" "consumerSec" OOB



reqUrl    = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
reqAuth   = signRq2 PLAINTEXT Nothing reqUrl >>= oauthRequest Browser

--foo = reqAuth >>= cliAskAuthorization authUrl

{-
response  = runOAuthM (fromApplication app) $ 
                 do { signRq2 PLAINTEXT Nothing reqUrl >>= oauthRequest Browser
                    ; cliAskAuthorization authUrl
                    ; signRq2 PLAINTEXT Nothing accUrl >>= oauthRequest Browser
                    ; signRq2 HMACSHA1 (Just $ Realm "realm") srvUrl >>= serviceRequest Browser
                    }
-}





main :: IO Int
main = do return 0

