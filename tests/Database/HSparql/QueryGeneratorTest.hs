{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.HSparql.QueryGeneratorTest ( testSuite ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.String.QQ as Str

import Database.HSparql.QueryGenerator

class CreateQuery a where
  createQuery :: a -> Text

instance CreateQuery (Query SelectQuery) where
  createQuery = T.pack . createSelectQuery

instance CreateQuery (Query ConstructQuery) where
  createQuery = T.pack . createConstructQuery

instance CreateQuery (Query AskQuery) where
  createQuery = T.pack . createAskQuery

instance CreateQuery (Query UpdateQuery) where
  createQuery = T.pack . createUpdateQuery

instance CreateQuery (Query DescribeQuery) where
  createQuery = T.pack . createDescribeQuery

normalizeWhitespace :: Text -> Text
normalizeWhitespace = T.strip
                      . T.replace "  " " "
                      . T.replace "  " " "
                      . T.replace "  " " "
                      . T.replace "\n" " "

queryTexts :: [(Text, Text)]
queryTexts =
  [ ( [Str.s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?x1 ?x2
WHERE {
  ?x0 dbprop:genre dbpedia:Web_browser .
  ?x0 foaf:name ?x1 .
  ?x0 foaf:page ?x2 .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        x    <- var
        name <- var
        page <- var

        triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
        triple_ x (foaf .:. "name") name
        triple_ x (foaf .:. "page") page

        selectVars [name, page]
    )

  , ( [Str.s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX example: <http://www.example.com/>
CONSTRUCT {
  ?x0 example:hasName ?x1 .
}
WHERE {
  ?x0 dbprop:genre dbpedia:Web_browser .
  ?x0 foaf:name ?x1 .
  ?x0 foaf:page ?x2 .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
        example  <- prefix "example" (iriRef "http://www.example.com/")

        x    <- var
        name <- var
        page <- var

        construct <- constructTriple x (example .:. "hasName") name

        triple_ x (dbpprop .:. "genre") (resource .:. "Web_browser")
        triple_ x (foaf .:. "name") name
        triple_ x (foaf .:. "page") page

        return ConstructQuery { queryConstructs = [construct] }
    )

  , ( [Str.s|
PREFIX dbpedia: <http://dbpedia.org/resource/> DESCRIBE dbpedia:Edinburgh WHERE {  }
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        uri <- describeIRI (resource .:. "Edinburgh")
        return DescribeQuery { queryDescribe = uri }
    )

  , ( [Str.s|
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop: <http://dbpedia.org/property/>
ASK {
  ?x0 dbprop:genre dbpedia:Web_browser .
}
|]
    , createQuery $ do
        resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
        dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

        x <- var
        ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

        return AskQuery { queryAsk = [ask] }
    )

  -- https://github.com/robstewart57/hsparql/pull/31
  , ( [Str.s|
PREFIX : <http://example1.com/>
PREFIX ex: <http://example2.com/>
SELECT ?x0 WHERE {
  ?x0 :property1 :X .
  FILTER ((?x1>(10))&&(?x1<(20))) .
  {
    SELECT ((AVG(?x0_0)) AS ?x1)
    WHERE {
      ?x0 :property2 [ex:property3 ?x0_0] .
    }
  }
}
|]
    , createQuery $ do
      p1 <- prefix "" (iriRef "http://example1.com/")
      s <- var
      v <- var
      triple_ s (p1 .:. "property1") (p1 .:. "X")
      filterExpr_ $ (v .>. (10::Integer)) .&&. (v .<. (20::Integer))
      subQuery_ $ do
        p2 <- prefix "ex" (iriRef "http://example2.com/")
        v' <- var
        triple_ s (p1 .:. "property2") [mkPredicateObject (p2 .:. "property3") v']
        select [avg v' `as` v]
      selectVars [s]
    )

  -- https://www.w3.org/TR/sparql11-query/#OptionalMatching
  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?x1 ?x2
WHERE {
  ?x0 foaf:name ?x1 .
  OPTIONAL { ?x0 foaf:mbox ?x2 . }
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        x <- var
        name <- var
        mbox <- var

        triple_ x (foaf .:. "name") name

        optional_ $ do
          triple_ x (foaf .:. "mbox") mbox

        selectVars [name, mbox]
    )

  -- https://www.w3.org/TR/sparql11-query/#alternatives
  , ( [Str.s|
PREFIX dc10: <http://purl.org/dc/elements/1.0/>
PREFIX dc11: <http://purl.org/dc/elements/1.1/>

SELECT ?x1
WHERE { { ?x0 dc10:title ?x1 . } UNION { ?x0 dc11:title ?x1 . } }
|]
    , createQuery $ do
        dc10     <- prefix "dc10" (iriRef "http://purl.org/dc/elements/1.0/")
        dc11     <- prefix "dc11" (iriRef "http://purl.org/dc/elements/1.1/")

        book <- var
        title <- var

        let tt10 = triple_ book (dc10 .:. "title") title
            tt11 = triple_ book (dc11 .:. "title") title
          in union_ tt10 tt11

        selectVars [title]
    )

  -- https://www.w3.org/TR/sparql11-query/#neg-notexists
  , ( [Str.s|
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0
WHERE
{
  ?x0 rdf:type foaf:Person .
  FILTER NOT EXISTS { ?x0 foaf:name ?x1 . }
}
|]
    , createQuery $ do
        rdf  <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        person <- var
        name <- var

        triple_ person (rdf .:. "type") (foaf .:. "Person")

        filterNotExists_ $ do
          triple_ person (foaf .:. "name") name

        selectVars [person]
    )

  , ( [Str.s|
PREFIX ex: <http://example.com/>

SELECT ?x0 ?x1
WHERE
{
  ?x0 ex:foo%20bar ?x1 .
}
|]
    , createQuery $ do
        ex <- prefix "ex" (iriRef "http://example.com/")

        s <- var
        o <- var

        triple_ s (ex .:. "foo bar") o

        selectVars [s, o]
    )

  -- Count distinct subjects and objects query
  , ( [Str.s|
SELECT ((COUNT(?x0)) AS ?x4)
WHERE
{
  {
    SELECT DISTINCT ?x0 WHERE {
      { ?x0 ?x2 ?x3 . } UNION
      { ?x1 ?x2 ?x0 . }
    }
  }
}
|]
    , createQuery $ do
        r <- var
        s <- var
        p <- var
        o <- var

        subQuery_ $ do
          let tt1 = triple_ r p o
              tt2 = triple_ s p r
           in do
             distinct_
             union_ tt1 tt2
             selectVars [r]

        countVar <- var
        select [count r `as` countVar]
    )

  -- Count number of instances of each class in the dataset.
  , ( [Str.s|
SELECT ?x1 ((COUNT(?x0)) AS ?x2) WHERE {
  ?x0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?x1 .
} GROUP BY ?x1
|]
    , createQuery $ do
        s <- var
        o <- var

        triple_ s (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") o

        groupBy_ o

        countVar <- var
        select [SelectVar o, count s `as` countVar]
    )

  -- Count number of resources typed with a class from Wikidata.
  , ( [Str.s|
SELECT ((COUNT(?x0)) AS ?x2) WHERE {
  ?x0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?x1 .
  FILTER (CONTAINS((STR(?x1)), "http://www.wikidata.org/entity")) .
}
|]
    , createQuery $ do
        s <- var
        o <- var

        triple_ s (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") o

        filterExpr_ $ contains (str o) ("http://www.wikidata.org/entity" :: Text)

        countVar <- var
        select [count s `as` countVar]
    )

  -- Create a federated query
  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?x1 WHERE {
  <http://example.org/myfoaf/I> foaf:knows ?x0 .
  SERVICE <http://people.example.org/sparql> {
    ?x0 foaf:name ?x1 .
  }
}
|]
    , createQuery $ do
        foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        person    <- var
        name      <- var

        triple_ (iriRef "http://example.org/myfoaf/I") (foaf .:. "knows") person

        service_ (iriRef "http://people.example.org/sparql") $ do
          triple_ person (foaf .:. "name") name

        selectVars [name]
    )

  -- Create query containing property paths
  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0 ?x1 WHERE {
  ?x0 ((foaf:knows/foaf:knows)/foaf:name) ?x1 .
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        s <- var
        o <- var

        let foafKnows = foaf .:. "knows"
        let foafName = foaf .:. "name"
        triple_ s (foafKnows .//. foafKnows .//. foafName) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0 ?x1 WHERE {
  ?x0 (^foaf:mbox) ?x1 .
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        s <- var
        o <- var

        let foafMbox = foaf .:. "mbox"
        triple_ s (inv foafMbox) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0 ?x1 WHERE {
  ?x0 (foaf:knows/(^foaf:knows)) ?x1 .
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        s <- var
        o <- var

        let foafKnows = foaf .:. "knows"
        triple_ s (foafKnows .//. inv foafKnows) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0 ?x1 WHERE {
  ?x0 (foaf:knows/(^foaf:knows)) ?x1 .
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        s <- var
        o <- var

        let foafKnows = foaf .:. "knows"
        triple_ s (foafKnows .//. inv foafKnows) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?x0 ?x1 WHERE {
  ?x0 ((foaf:knows+)/foaf:name) ?x1 .
}
|]
    , createQuery $ do
        foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

        s <- var
        o <- var

        let foafKnows = foaf .:. "knows"
        let foafName = foaf .:. "name"
        triple_ s ((foafKnows +.) .//. foafName) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX ex: <http://example.com/>

SELECT ?x0 ?x1 WHERE {
  ?x0 ((ex:motherOf|ex:fatherOf)+) ?x1 .
}
|]
    , createQuery $ do
        ex     <- prefix "ex" (iriRef "http://example.com/")

        s <- var
        o <- var

        let exMotherOf = ex .:. "motherOf"
        let exFatherOf = ex .:. "fatherOf"
        triple_ s ((exMotherOf .|. exFatherOf) +.) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX ex: <http://example.com/>

SELECT ?x0 ?x1 WHERE {
  ?x0 ((ex:motherOf|ex:fatherOf)+) ?x1 .
}
|]
    , createQuery $ do
        ex     <- prefix "ex" (iriRef "http://example.com/")

        s <- var
        o <- var

        let exMotherOf = ex .:. "motherOf"
        let exFatherOf = ex .:. "fatherOf"
        triple_ s ((exMotherOf .|. exFatherOf) +.) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?x0 ?x1 WHERE {
  ?x0 (rdf:type/(rdfs:subClassOf*)) ?x1 .
}
|]
    , createQuery $ do
        rdf  <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

        s <- var
        o <- var

        let rdfType = rdf .:. "type"
        let rdfsSubClassOf = rdfs .:. "subClassOf"
        triple_ s (rdfType .//. (rdfsSubClassOf *.)) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?x0 ?x1 WHERE {
  ?x0 (a/(rdfs:subClassOf*)) ?x1 .
}
|]
    , createQuery $ do
        rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

        s <- var
        o <- var

        let rdfsSubClassOf = rdfs .:. "subClassOf"
        triple_ s (a .//. (rdfsSubClassOf *.)) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?x0 ?x1 WHERE {
  ?x0 (a/(rdfs:subClassOf?)) ?x1 .
}
|]
    , createQuery $ do
        rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

        s <- var
        o <- var

        let rdfsSubClassOf = rdfs .:. "subClassOf"
        triple_ s (a .//. (rdfsSubClassOf ?.)) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?x0 ?x1 WHERE {
  ?x0 !(rdf:type|rdfs:subClassOf|a) ?x1 .
}
|]
    , createQuery $ do
        rdf  <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")

        s <- var
        o <- var

        let rdfType = rdf .:. "type"
        let rdfsSubClassOf = rdfs .:. "subClassOf"
        triple_ s (neg $ rdfType ..|.. rdfsSubClassOf ..|.. a) o

        selectVars [s, o]
    )

  , ( [Str.s|
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?x0 ?x1 WHERE {
  ?x0 !(rdf:type|^rdf:type) ?x1 .
}
|]
    , createQuery $ do
        rdf  <- prefix "rdf" (iriRef "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

        s <- var
        o <- var

        let rdfType = rdf .:. "type"
        triple_ s (neg $ rdfType ..|.. inv' rdfType) o

        selectVars [s, o]
    )
  ]

testSuite :: [Test.Framework.Test]
testSuite = [
    testGroup "Database.HSparql.QueryGenerator tests" $ (`map` queryTexts) $
      \(expected, actual) -> testCase (T.unpack expected) $ do
        assertEqual "" (normalizeWhitespace expected) (normalizeWhitespace actual)
  ]

