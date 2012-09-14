{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status500)

import Control.Concurrent (forkIO)

import Test.Framework

import Database.HSparql.ConnectionTest

main =
  do forkIO $ startServer
     ropts <- interpretArgsOrExit []
     defaultMainWithOpts tests ropts
     where tests = Database.HSparql.ConnectionTest.testSuite

startServer :: IO ()
startServer = run 3000 testServer
              where testServer req = return $ response req
                    response req = case rawQueryString req of
                                        "?query=PREFIX%20foaf%3A%20%3Chttp%3A%2F%2Fxmlns.com%2Ffoaf%2F0.1%2F%3E%20PREFIX%20dbpedia%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fproperty%2F%3E%20PREFIX%20dbprop%3A%20%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%20SELECT%20%20%3Fx1%20WHERE%20%7B%3Fx0%20dbpedia%3Agenre%20dbprop%3AWeb_browser%20.%20%3Fx0%20foaf%3Aname%20%3Fx1%20.%7D"
                                                -> selectResponse
                                        otherwise
                                                -> errorResponse
                    selectResponse = ResponseFile status200 [("Content-Type", "application/xml")] "tests/fixtures/sparql_select_response.xml" Nothing
                    errorResponse = responseLBS status500 [] ""
