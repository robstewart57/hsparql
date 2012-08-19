module DBPedia where

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple)

selectExample :: IO ()
selectExample = do
  (Just s) <- selectQuery "http://dbpedia.org/sparql" simpleSelect
  putStrLn . show $ s

askExample :: IO ()
askExample = do
  res <- askQuery "http://dbpedia.org/sparql" simpleAsk
  putStrLn $ "result: " ++ (show (res::Bool))

describeExample :: IO ()
describeExample = do
  rdfGraph <- describeQuery "http://dbpedia.org/sparql" simpleDescribe
  mapM_ print (triplesOf rdfGraph)

constructExample :: IO ()
constructExample = do
  rdfGraph <- constructQuery "http://dbpedia.org/sparql" simpleConstruct
  mapM_ print (triplesOf rdfGraph)


simpleSelect :: Query SelectQuery
simpleSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

    x    <- var
    name <- var

    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
    triple x (foaf .:. "name") name

    return SelectQuery { queryVars = [name] }


simpleConstruct :: Query ConstructQuery
simpleConstruct = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
    example  <- prefix "example" (iriRef "http://www.example.com/")

    x    <- var
    name <- var
    page <- var

    construct <- constructTriple x (example .:. "hasName") name
    
    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
    triple x (foaf .:. "name") name
    triple x (foaf .:. "page") page

    return ConstructQuery { queryConstructs = [construct] }


simpleAsk :: Query AskQuery
simpleAsk = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

    x <- var
    ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

    return AskQuery { queryAsk = [ask] }


simpleDescribe :: Query DescribeQuery
simpleDescribe = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    uri <- describeIRI (resource .:. "Edinburgh")
    return DescribeQuery { queryDescribe = uri }


trickySelect :: Query SelectQuery
trickySelect = do
    resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
    owl      <- prefix "owl" (iriRef "http://www.w3.org/2002/07/owl#")

    x     <- var
    name  <- var
    fbase <- var
    page  <- var

    -- Identify
    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")

    -- Query
    triple x (foaf .:. "name") name
    optional $ do triple x (owl .:. "sameAs") fbase
                  filterExpr $ regex fbase "freebase"
    filterExpr $ notExpr $ bound fbase

    triple x (foaf .:. "page") page

    distinct

    orderNext name
    orderNextDesc fbase

    return SelectQuery { queryVars =  [x, name, page, fbase] }

frenchFilmsSelect :: Query SelectQuery
frenchFilmsSelect = do
    skos <- prefix "skos" (iriRef "http://www.w3.org/2004/02/skos/core#")
    film <- var
    triple film (skos .:. "subject") (iriRef "http://dbpedia.org/resource/Category:French_films")
    return SelectQuery { queryVars = [film] }

fpsSelect :: Query SelectQuery
fpsSelect = do
    property  <- var
    hasValue  <- var
    isValueOf <- var

    union (triple (iriRef "http://dbpedia.org/resource/Category:First-person_shooters") property hasValue)
          (triple isValueOf property (iriRef "http://dbpedia.org/resource/Category:First-person_shooters"))


    return SelectQuery { queryVars = [isValueOf] }

berlinersSelect :: Query SelectQuery
berlinersSelect = do
    xsd  <- prefix "xsd" (iriRef "http://www.w3.org/2001/XMLSchema#")
    prop <- prefix "prop" (iriRef "http://dbpedia.org/property/")
    dbo  <- prefix "dbo" (iriRef "http://dbpedia.org/ontology/")
    foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
    resc <- prefix "resc" (iriRef "http://dbpedia.org/resource/")

    name     <- var
    birth    <- var
    death    <- var
    person   <- var
    knownfor <- var

    triple person (prop .:. "birthPlace") (resc .:. "Berlin")
    triple person (dbo  .:. "birthdate")  birth
    triple person (foaf .:. "name")       name
    triple person (dbo  .:. "deathdate")  death

    filterExpr $ birth .<. ("1900-01-01", xsd .:. "date")

    optional $ triple person (prop .:. "KnownFor") knownfor

    return SelectQuery { queryVars = [name, birth, death, person, knownfor] }
