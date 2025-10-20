{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBPedia where

import Data.RDF hiding (Query, triple)
import Data.Text hiding (show)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

selectExample :: IO ()
selectExample = do
  (Just s) <- selectQuery "http://dbpedia.org/sparql" simpleSelect
  print s

selectWithLiteralExample :: IO ()
selectWithLiteralExample = do
  (Just s) <- selectQuery "http://dbpedia.org/sparql" simpleSelectWithLiteral
  putStrLn . show $ s

askExample :: IO ()
askExample = do
  res <- askQuery "http://dbpedia.org/sparql" simpleAsk
  putStrLn $ "result: " ++ show (res :: Bool)

describeExample :: IO ()
describeExample = do
  (rdfGraph :: RDF TList) <- describeQuery "http://dbpedia.org/sparql" simpleDescribe
  mapM_ print (triplesOf rdfGraph)

constructExample :: IO ()
constructExample = do
  (rdfGraph :: RDF TList) <- constructQuery "http://dbpedia.org/sparql" simpleConstruct
  mapM_ print (triplesOf rdfGraph)

simpleSelect :: Query SelectQuery
simpleSelect = do
  resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
  dbpprop <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
  foaf' <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

  x <- var
  name <- var

  triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
  triple_ x (foaf' .:. "name") name

  selectVars [name]

simpleSelectWithLiteral :: Query SelectQuery
simpleSelectWithLiteral = do
  ontology <- prefix "dbpedia" (iriRef "http://dbpedia.org/ontology/")
  rdf' <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  rdfs' <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

  l <- var

  triple_ l (rdf' .:. "type") (ontology .:. "ProgrammingLanguage")
  triple_ l (rdfs' .:. "label") (("D (programming language)", "en") :: (Text, Text))

  selectVars [l]

simpleConstruct :: Query ConstructQuery
simpleConstruct = do
  resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
  dbpprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
  foaf' <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  example <- prefix "example" (iriRef "http://www.example.com/")

  x <- var
  name <- var

  construct <- constructTriple x (example .:. "hasName") name

  triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
  triple_ x (foaf' .:. "name") name

  return ConstructQuery {queryConstructs = [construct]}

simpleAsk :: Query AskQuery
simpleAsk = do
  resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
  dbprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

  x <- var
  ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

  return AskQuery {queryAsk = [ask]}

simpleDescribe :: Query DescribeQuery
simpleDescribe = do
  resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
  uri <- describeIRI (resource .:. "Edinburgh")
  return DescribeQuery {queryDescribe = uri}

trickySelect :: Query SelectQuery
trickySelect = do
  resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
  dbpprop <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
  foaf' <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  owl' <- prefix "owl" (iriRef "http://www.w3.org/2002/07/owl#")

  x <- var
  name <- var
  fbase <- var

  -- Identify
  triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")

  -- Query
  triple_ x (foaf' .:. "name") name
  optional_ $ do
    triple_ x (owl' .:. "sameAs") fbase
    filterExpr_ $ regex fbase ("freebase" :: Text)
  filterExpr_ $ notExpr $ bound fbase

  distinct_

  orderNext name
  orderNextDesc fbase

  selectVars [x, name, fbase]

frenchFilmsSelect :: Query SelectQuery
frenchFilmsSelect = do
  skos' <- prefix "skos" (iriRef "http://www.w3.org/2004/02/skos/core#")
  film <- var
  triple_ film (skos' .:. "subject") (iriRef "http://dbpedia.org/resource/Category:French_films")
  selectVars [film]

-- return SelectQuery {queryExpr = [film]}

fpsSelect :: Query SelectQuery
fpsSelect = do
  property <- var
  hasValue <- var
  isValueOf <- var

  union_
    (triple_ (iriRef "http://dbpedia.org/resource/Category:First-person_shooters") property hasValue)
    (triple_ isValueOf property (iriRef "http://dbpedia.org/resource/Category:First-person_shooters"))

  selectVars [isValueOf]

berlinersSelect :: Query SelectQuery
berlinersSelect = do
  xsd' <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")
  prop <- prefix "prop" (iriRef "http://dbpedia.org/property/")
  dbo <- prefix "dbo" (iriRef "http://dbpedia.org/ontology/")
  foaf' <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  resc <- prefix "resc" (iriRef "http://dbpedia.org/resource/")

  name <- var
  birth <- var
  death <- var
  person <- var
  knownfor <- var

  triple_ person (prop .:. "birthPlace") (resc .:. "Berlin")
  triple_ person (dbo .:. "birthDate") birth
  triple_ person (foaf' .:. "name") name
  triple_ person (dbo .:. "deathDate") death

  filterExpr_ $ birth .<. ("1900-01-01" :: Text, xsd' .:. "date")

  optional_ $ triple_ person (prop .:. "KnownFor") knownfor

  selectVars [name, birth, death, person, knownfor]
