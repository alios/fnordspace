{-# LANGUAGE NoMonomorphismRestriction #-}

{-
Copyright (c) 2011 fnordspace labs 
All rights reserved.

written by Markus Barenhoff <alios@alios.org>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Markus Barenhoff nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Network.Twitter () where

import Data.Maybe (fromJust)
import Control.Monad.Cont (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Browser

consumerKey = "mJKuzrfsLKrccB4SuHRnfw"
consumerSec = "EwvmSWYidl6pf7bJVyxMGMQ4UbggkUSN5IZLmrQsYs"

requestTokenURL = "https://api.twitter.com/oauth/request_token"  
accessTokenURL = "https://api.twitter.com/oauth/access_token"
authorizeURL = "https://api.twitter.com/oauth/authorize"

  
reqUrl     = (fromJust . parseURL $ requestTokenURL)
reqUrlPost = reqUrl { method = POST, reqPayload = BS.pack consumerSec}
accUrl    = fromJust . parseURL $ accessTokenURL
srvUrl    = fromJust . parseURL $ "http://service/path/to/resource/"
authUrl   = ((authorizeURL ++ "?oauth_token=")++) . findWithDefault ("oauth_token","ERROR") . oauthParams
app       = Application consumerKey consumerSec OOB
response  = runOAuthM (fromApplication app) $ 
            do { browser <- mkBrowserM
               ; signRq2 HMACSHA1 Nothing reqUrlPost >>= oauthRequest browser
               ; cliAskAuthorization authUrl
               ; signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest browser
               ; signRq2 HMACSHA1 Nothing srvUrl >>= serviceRequest browser
               }

