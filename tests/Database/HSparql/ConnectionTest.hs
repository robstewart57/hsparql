{-# LANGUAGE FlexibleInstances #-}

module Database.HSparql.ConnectionTest ( testSuite ) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Text as T
import qualified Data.RDF as RDF

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

testSuite = [
    testGroup "Connection tests" [
      testCase "selectQuery" test_selectQuery
    ]
  ]

test_selectQuery =
  let expectedBVars = Just [ [ Bound $ RDF.lnode $ RDF.plainLL (T.pack "Kazehakase") (T.pack "en") ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL (T.pack "Netscape Browser") (T.pack "en") ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL (T.pack "SlimBrowser") (T.pack "en") ]
                           ]
  in do
    bvars <- selectQuery endPoint query
    assertEqual "bound variables" expectedBVars bvars

    where endPoint = "http://localhost:3000"
          query = do
              resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

              x    <- var
              name <- var

              triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              triple x (foaf .:. "name") name

              return SelectQuery { queryVars = [name] }
