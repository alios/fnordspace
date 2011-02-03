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

module Network.OAuth.Http.Browser (mkBrowser, mkBrowserM) where

import Data.Maybe (fromJust)
import Control.Monad.Cont (liftIO)

import Network.URI
import Network.HTTP
import Network.Browser
import Data.ByteString.Lazy (ByteString)

import Network.OAuth.Http.HttpClient
import qualified Network.OAuth.Http.Request as R
import qualified Network.OAuth.Http.Response as Re

type BrowserS = BrowserState (HandleStream ByteString)

-- | 'Browser' is a hoauth 'HttpClient' implementation using "Network.Browser"
data Browser = Browser BrowserS

-- | Create a new 'Browser' instance
mkBrowser = 
  do state <- browse getBrowserState
     return $ Browser state 

-- | general monadic version of 'mkBrowser'
mkBrowserM = liftIO mkBrowser

instance HttpClient Browser where  
  runClient (Browser state) = liftIO.(runBrowserClient state)

-- | implementation of the 'runClient' for "Network.Browser"
runBrowserClient :: BrowserS -> R.Request -> IO (Either String Re.Response)
runBrowserClient state r =
  let action = withBrowserState state $ request $ req2req r
  in do (uri, resp) <- browse  action
        let (ra,rb,rc) = rspCode resp
            status = ra * 100 + rb * 10 + rc
            reason = rspReason resp
            headers = R.fromList $ 
                      map (\h -> (show $ hdrName h, hdrValue h)) $ 
                      rspHeaders resp
            payload = rspBody resp
        return $ Right $ Re.RspHttp status reason headers payload
  

req2req :: R.Request -> Request ByteString
req2req req = 
  let uri = fromJust $ parseURI $ R.showURL req 
      method = case (R.method req) of
        R.GET -> GET
        R.POST  -> POST
        R.PUT -> PUT
        R.DELETE -> DELETE
        R.TRACE -> TRACE
        R.CONNECT -> CONNECT
        R.HEAD -> HEAD
      headers = map (\(k,v) -> mkHeader (HdrCustom k) v) 
                $ R.toList $ R.reqHeaders req
      body = R.reqPayload req
  in Request uri method headers body




