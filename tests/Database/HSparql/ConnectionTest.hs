
module Database.HSparql.ConnectionTest ( testSuite ) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.RDF as RDF
import qualified Data.RDF.TriplesGraph as G

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

testSuite = [
    testGroup "Database.HSparql.Connection tests" [
      testCase "selectQuery" test_selectQuery
      , testCase "askQuery" test_askQuery
      , testCase "constructQuery" test_constructQuery
      , testCase "describeQuery" test_describeQuery
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

test_askQuery = do
    bool <- askQuery endPoint query
    assertBool "invalid" bool

    where endPoint = "http://localhost:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

              x <- var
              ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

              return AskQuery { queryAsk = [ask] }

test_constructQuery =
  let expectedGraph :: G.TriplesGraph
      expectedGraph = G.mkRdf expectedTriples Nothing (RDF.PrefixMappings Map.empty)
      expectedTriples = [ RDF.Triple (RDF.unode $ T.pack "http://dbpedia.org/resource/Kazehakase")
                                     (RDF.unode $ T.pack "http://www.example.com/hasName")
                                     (RDF.lnode $ RDF.plainLL (T.pack "Kazehakase") (T.pack "en")) ]
  in do
    graph <- constructQuery endPoint query :: IO G.TriplesGraph
    assertBool "RDF does not include the constructed triple" $ RDF.isIsomorphic expectedGraph graph

    where endPoint = "http://localhost:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
              example  <- prefix "example" (iriRef "http://www.example.com/")

              x    <- var
              name <- var

              construct <- constructTriple x (example .:. "hasName") name
          
              triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              triple x (foaf .:. "name") name

              return ConstructQuery { queryConstructs = [construct] }

test_describeQuery =
  let expectedNode = RDF.unode $ T.pack "http://dbpedia.org/resource/Edinburgh"
  in do
    graph <- describeQuery endPoint query :: IO G.TriplesGraph
    assertBool "RDF does not include the required node" $ RDF.rdfContainsNode graph expectedNode

    where endPoint = "http://localhost:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              uri <- describeIRI (resource .:. "Edinburgh")
              return DescribeQuery { queryDescribe = uri }
