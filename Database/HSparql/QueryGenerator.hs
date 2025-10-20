{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | The query generator DSL for SPARQL, used when connecting to remote
--   endpoints.
module Database.HSparql.QueryGenerator
  ( -- * Creating Queries
    createSelectQuery,
    createConstructQuery,
    createAskQuery,
    createUpdateQuery,
    createDescribeQuery,

    -- * Query Actions
    prefix,
    var,
    embeddedTriple,
    Database.HSparql.QueryGenerator.triple,
    triple_,
    mkPredicateObject,
    constructTriple,
    constructTriple_,
    askTriple,
    askTriple_,
    updateTriple,
    updateTriple_,
    describeIRI,
    describeIRI_,
    optional,
    optional_,
    service,
    service_,
    union,
    union_,
    filterExpr,
    filterExpr_,
    filterExists,
    filterExists_,
    filterNotExists,
    filterNotExists_,
    bind,
    bind_,
    subQuery,
    subQuery_,
    select,
    selectVars,
    as,

    -- ** Property paths

    -- | SPARQL 1.1 property paths documentation: https://www.w3.org/TR/sparql11-query/#propertypaths
    a,

    -- *** Binary operators

    -- **** Property path binary operators
    (.//.),
    (.|.),

    -- **** Negative property set binary operators
    (..|..),

    -- *** Unary operators

    -- **** Property path unary operators
    inv,
    (*.),
    (+.),
    (?.),

    -- **** Negative property set unary operators
    neg,
    inv',

    -- ** Duplicate handling
    distinct,
    distinct_,
    reduced,
    reduced_,

    -- ** Limit handling
    limit,
    limit_,

    -- ** Groups handling
    groupBy,
    groupBy_,
    having,
    having_,

    -- ** Order handling
    orderNext,
    orderNextAsc,
    orderNextDesc,

    -- ** Auxiliary
    (.:.),
    iriRef,

    -- * Term Manipulation

    -- ** Operations
    (.+.),
    (.-.),
    (.*.),
    (./.),
    (.&&.),
    (.||.),

    -- ** Relations
    (.==.),
    (.!=.),
    (.<.),
    (.>.),
    (.<=.),
    (.>=.),

    -- ** Negation
    notExpr,

    -- ** Builtin aggregation functions
    count,
    sum_,
    min_,
    max_,
    avg,
    groupConcat,

    -- ** Builtin Functions
    str,
    lang,
    langMatches,
    datatype,
    bound,
    sameTerm,
    isIRI,
    isURI,
    isBlank,
    isLiteral,
    regex,
    regexOpts,
    strlen,
    substr,
    ucase,
    lcase,
    strstarts,
    strends,
    contains,
    containsWith,
    strbefore,
    strafter,
    abs_,
    round_,
    ceil,
    floor_,
    concat_,
    replace,
    rand,

    -- * Printing Queries
    qshow,

    -- * Types
    Query,
    Prefix,
    Variable,
    VarOrNode (..),
    BlankNodePattern,
    Pattern,
    SelectQuery (..),
    SelectExpr (..),
    ConstructQuery (..),
    AskQuery (..),
    UpdateQuery (..),
    DescribeQuery (..),
    IRIRef (..),

    -- * Classes
    TermLike (..),
    SubjectTermLike,
    PredicateTermLike,
    ObjectTermLike,
  )
where

import Control.Monad (void)
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List (intercalate, intersperse)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.RDF as RDF
import qualified Data.Text as T
import qualified Network.URI as URI

-- State monads

-- | The 'State' monad applied to 'QueryData'.
type Query a = State QueryData a

-- | Execute a 'Query' action, starting with initial 'QueryData', then process
--  the resulting 'QueryData'.
execQuery :: QueryData -> Query a -> (QueryData -> b) -> b
execQuery qd q f = f $ execState q qd

-- | Execute a 'Query' action, starting with the empty 'queryData', then process
--  the resulting 'QueryData'.
execQuery0 :: Query a -> (QueryData -> b) -> b
execQuery0 = execQuery queryData

-- | Execute a 'Select Query' action, returning the 'String' representation of the query.
createSelectQuery :: Query SelectQuery -> String
createSelectQuery q = execQuery0 (specifyVars q) qshow

specifyVars :: Query SelectQuery -> Query ()
specifyVars q = do
  query <- q
  modify $ \s -> s {vars = queryExpr query, queryType = SelectType}

-- | Execute a 'Construct Query' action, returning the 'String' representation of the query.
createConstructQuery :: Query ConstructQuery -> String
createConstructQuery q = execQuery0 specifyType qshow
  where
    specifyType :: Query ()
    specifyType = do
      query <- q
      modify $ \s -> s {constructTriples = queryConstructs query, queryType = ConstructType}

-- | Execute a 'Ask Query' action, returning the 'String' representation of the query.
createAskQuery :: Query AskQuery -> String
createAskQuery q = execQuery0 specifyType qshow
  where
    specifyType :: Query ()
    specifyType = do
      query <- q
      modify $ \s -> s {askTriples = queryAsk query, queryType = AskType}

-- | Execute a 'Update Query' action, returning the 'String' representation of the query.
createUpdateQuery :: Query UpdateQuery -> String
createUpdateQuery q = execQuery0 specifyType qshow
  where
    specifyType :: Query ()
    specifyType = do
      query <- q
      modify $ \s -> s {updateTriples = queryUpdate query, queryType = UpdateType}

-- | Execute a 'Describe Query' action, returning the 'String' representation of the query.
createDescribeQuery :: Query DescribeQuery -> String
createDescribeQuery q = execQuery0 specifyType qshow
  where
    specifyType :: Query ()
    specifyType = do
      query <- q
      modify $ \s -> s {describeURI = Just (queryDescribe query), queryType = DescribeType}

-- Manipulate data within monad

-- | Add a prefix to the query, given an IRI reference, and return it.
prefix :: T.Text -> IRIRef -> Query Prefix
prefix pre (AbsoluteIRI node) = do
  let p = Prefix pre node
  modify $ \s -> s {prefixes = p : prefixes s}
  return p
prefix _ _ = error "prefix requires an absolute IRI"

-- | Create and return a variable to the query, usable in later expressions.
var :: Query Variable
var = do
  qis <- gets (NE.init . subQueryIdx)
  n <- gets varsIdx
  let sqis = NE.fromList (qis ++ [n])
  modify $ \s -> s {varsIdx = n + 1}
  return $ Variable sqis

-- | Create an embedded triple usable in an expression.
--   See SPARQL* at <https://wiki.blazegraph.com/wiki/index.php/Reification_Done_Right>
--   or <https://arxiv.org/abs/1406.3399>.
embeddedTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> EmbeddedTriple
embeddedTriple x y z = EmbeddedTriple $ EmbeddedTriple' (varOrTerm x) (varOrTerm y) (varOrTerm z)

-- | Restrict the query to only results for which values match constants in this
--   triple, or for which the variables can be bound.
triple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
triple x y z = do
  let t = QTriple (varOrTerm x) (varOrTerm y) (varOrTerm z)
  modify $ \s -> s {pattern = appendPattern t (pattern s)}
  return t

triple_ :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query ()
triple_ x y z = void $ triple x y z

constructTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
constructTriple x y z = do
  let t = QTriple (varOrTerm x) (varOrTerm y) (varOrTerm z)
  modify $ \s -> s {constructTriples = appendTriple t (constructTriples s)}
  return t

constructTriple_ :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query ()
constructTriple_ x y z = void $ constructTriple x y z

askTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
askTriple x y z = do
  let t = QTriple (varOrTerm x) (varOrTerm y) (varOrTerm z)
  modify $ \s -> s {askTriples = appendTriple t (askTriples s)}
  return t

askTriple_ :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query ()
askTriple_ x y z = void $ askTriple x y z

updateTriple :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query Pattern
updateTriple x y z = do
  let t = QTriple (varOrTerm x) (varOrTerm y) (varOrTerm z) -- TODO: should only allow terms
  modify $ \s -> s {updateTriples = appendTriple t (updateTriples s)}
  return t

updateTriple_ :: (SubjectTermLike a, PredicateTermLike b, ObjectTermLike c) => a -> b -> c -> Query ()
updateTriple_ x y z = void $ updateTriple x y z

describeIRI :: IRIRef -> Query IRIRef
describeIRI newIri = do
  modify $ \s -> s {describeURI = Just newIri}
  return newIri

describeIRI_ :: IRIRef -> Query ()
describeIRI_ = void . describeIRI

selectVars :: [Variable] -> Query SelectQuery
selectVars vs = return SelectQuery {queryExpr = fmap SelectVar vs}

select :: [SelectExpr] -> Query SelectQuery
select es = return SelectQuery {queryExpr = es}

-- | Add optional constraints on matches. Variable bindings within the optional
--   action are lost, so variables must always be defined prior to opening the
--   optional block.
optional :: Query a -> Query Pattern
optional q = do
  -- Determine the patterns by executing the action on a blank QueryData, and
  -- then pulling the patterns out from there.
  let option = execQuery0 q $ OptionalGraphPattern . pattern
  modify $ \s -> s {pattern = appendPattern option (pattern s)}
  return option

optional_ :: Query a -> Query ()
optional_ = void . optional

-- | Instruct a federated query processor to invoke the portion of a SPARQL
--  query against a remote SPARQL endpoint.
--
--  For example
--
--  > createQuery $ do
--  >   foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
--  >
--  >   person    <- var
--  >   name      <- var
--  >
--  >   triple_ (iriRef "http://example.org/myfoaf/I") (foaf .:. "knows") person
--  >
--  >   _ <- service (iriRef "http://people.example.org/sparql") $ do
--  >     triple_ person (foaf .:. "name") name
--  >
--  >   selectVars [name]
--
--   produces the SPARQL query:
--
--  > PREFIX foaf: <http://xmlns.com/foaf/0.1/>
--  > SELECT ?x1 WHERE {
--  >   <http://example.org/myfoaf/I> foaf:knows ?x0 .
--  >   SERVICE <http://people.example.org/sparql> {
--  >     ?x0 foaf:name ?x1 .
--  >   }
--  > }
service ::
  -- | SPARQL endpoint
  IRIRef ->
  -- | SPARQL query to invoke against a remote SPARQL endpoint
  Query a ->
  Query Pattern
service endpoint q = do
  -- Determine the patterns by executing the action on a blank QueryData, and
  -- then pulling the patterns out from there.
  let servicePatternGroup = execQuery0 q $ ServiceGraphPattern endpoint . pattern
  modify $ \s -> s {pattern = appendPattern servicePatternGroup (pattern s)}
  return servicePatternGroup

-- | Same as 'service', but without returning the query patterns.
service_ :: IRIRef -> Query a -> Query ()
service_ q = void . service q

-- | Add a union structure to the query pattern. As with 'optional' blocks,
--   variables must be defined prior to the opening of any block.
union :: Query a -> Query b -> Query Pattern
union q1 q2 = do
  let p1 = execQuery0 q1 pattern
      p2 = execQuery0 q2 pattern
      union' = UnionGraphPattern p1 p2
  modify $ \s -> s {pattern = appendPattern union' (pattern s)}
  return union'

union_ :: Query a -> Query b -> Query ()
union_ x y = void $ union x y

filterExists :: Query a -> Query Pattern
filterExists q = do
  let p = execQuery0 q pattern
      filterExists' = FilterExistsPattern p
  modify $ \s -> s {pattern = appendPattern filterExists' (pattern s)}
  return filterExists'

filterExists_ :: Query a -> Query ()
filterExists_ = void . filterExists

filterNotExists :: Query a -> Query Pattern
filterNotExists q = do
  let p = execQuery0 q pattern
      filterNotExists' = FilterNotExistsPattern p
  modify $ \s -> s {pattern = appendPattern filterNotExists' (pattern s)}
  return filterNotExists'

filterNotExists_ :: Query a -> Query ()
filterNotExists_ = void . filterNotExists

-- | Restrict results to only those for which the given expression is true.
filterExpr :: (TermLike a) => a -> Query Pattern
filterExpr e = do
  let f = Filter (expr e)
  modify $ \s -> s {pattern = appendPattern f (pattern s)}
  return f

filterExpr_ :: (TermLike a) => a -> Query ()
filterExpr_ = void . filterExpr

-- | Bind the result of an expression to a variable.
bind :: Expr -> Variable -> Query Pattern
bind e v = do
  let b = Bind (expr e) v
  modify $ \s -> s {pattern = appendPattern b (pattern s)}
  return b

bind_ :: Expr -> Variable -> Query ()
bind_ x y = void $ bind x y

-- | Perform a subquery.
subQuery :: Query SelectQuery -> Query Pattern
subQuery q = do
  -- Manage subquery indexes
  qis <- gets subQueryIdx
  let sqis = qis |> 0
      qis' = NE.fromList $ NE.init qis ++ [NE.last qis + 1]
  -- Execute the subquery action
  let subQueryData0 = queryData {subQueryIdx = sqis}
      subQueryData = execQuery subQueryData0 (specifyVars q) id
  -- Merge prefixes
  prefixesParentQuery <- gets prefixes
  let prefixesSubQuery = prefixes subQueryData
      newPrefixes = prefixesSubQuery `L.union` prefixesParentQuery
  -- Create the subquery pattern and remove prefixes from the subquery
  let sq = SubQuery $ subQueryData {prefixes = []}
  -- Append the subquery pattern, update the subquery index and the prefixes.
  modify $ \s ->
    s
      { pattern = appendPattern sq (pattern s),
        subQueryIdx = qis',
        prefixes = newPrefixes
      }
  return sq

subQuery_ :: Query SelectQuery -> Query ()
subQuery_ = void . subQuery

-- Random auxiliary

-- | Form a 'Node', with the 'Prefix' and reference name.
(.:.) :: Prefix -> T.Text -> IRIRef
(.:.) = PrefixedName

-- Duplicate handling

-- | Set duplicate handling to 'Distinct'. By default, there are no reductions.
distinct :: Query Duplicates
distinct = do
  modify $ \s -> s {duplicates = Distinct}
  gets duplicates

distinct_ :: Query ()
distinct_ = void distinct

-- | Set duplicate handling to 'Reduced'. By default, there are no reductions.
reduced :: Query Duplicates
reduced = do
  modify $ \s -> s {duplicates = Reduced}
  gets duplicates

reduced_ :: Query ()
reduced_ = void reduced

-- | Set limit handling to the given value.  By default, there are no limits.
--   Note: negative numbers cause no query results to be returned.
limit :: Int -> Query Limit
limit n = do
  modify $ \s -> s {limits = Limit n}
  gets limits

limit_ :: Int -> Query ()
limit_ = void . limit

-- Grouping

-- | Divide the solution into one or more groups.
groupBy :: (TermLike a) => a -> Query [GroupBy]
groupBy e = do
  modify $ \s -> s {groups = groups s ++ [GroupBy . expr $ e]}
  gets groups

groupBy_ :: (TermLike a) => a -> Query ()
groupBy_ = void . groupBy

having :: (TermLike a) => a -> Query Having
having e = do
  modify $ \s -> s { havings = Just . Having . expr $ e }
  fromJust <$> gets havings

having_ :: (TermLike a) => a -> Query ()
having_ = void . having

-- Order handling

-- | Alias of 'orderNextAsc'.
orderNext :: (TermLike a) => a -> Query ()
orderNext = orderNextAsc

-- | Order the results, after any previous ordering, based on the term, in
--   ascending order.
orderNextAsc :: (TermLike a) => a -> Query ()
orderNextAsc x = modify $ \s -> s {ordering = ordering s ++ [Asc $ expr x]}

-- | Order the results, after any previous ordering, based on the term, in
--   descending order.
orderNextDesc :: (TermLike a) => a -> Query ()
orderNextDesc x = modify $ \s -> s {ordering = ordering s ++ [Desc $ expr x]}

-- | Permit variables and values to seemlessly be put into argument for 'triple'
--   and similar functions
class TermLike a where
  varOrTerm :: a -> VarOrTerm

  expr :: a -> Expr
  expr = VarOrTermExpr . varOrTerm

instance TermLike Variable where
  varOrTerm = Var

instance TermLike IRIRef where
  varOrTerm = Term . IRIRefTerm

instance TermLike PropertyPathExpr where
  varOrTerm = Term . PropertyPathTerm

instance TermLike EmbeddedTriple where
  varOrTerm (EmbeddedTriple v) = v

instance TermLike BlankNodePattern where
  varOrTerm [] = Term (BNode Nothing)
  varOrTerm xs = BlankNodePattern' xs

  expr [] = error "FIXME: blank node expression"
  expr _ = error "cannot use a blank node pattern as an expression"

instance TermLike Expr where
  varOrTerm = error "cannot use an expression as a term"
  expr = id

instance TermLike Integer where
  varOrTerm = Term . NumericLiteralTerm
  expr = NumericExpr . NumericLiteralExpr

instance TermLike T.Text where
  varOrTerm = Term . RDFLiteralTerm . RDF.plainL

-- | Represent a literal with a language. Ex. "earth"@en
instance TermLike (T.Text, T.Text) where
  varOrTerm (s, lang') = Term . RDFLiteralTerm $ RDF.plainLL s lang'

-- | Represent a literal with its type. Ex. "1"^^xsd:integer
instance TermLike (T.Text, IRIRef) where
  varOrTerm (s, ref) = Term . RDFLiteralTerm $ RDF.typedL s (getFQN ref)

instance TermLike Bool where
  varOrTerm = Term . BooleanLiteralTerm

instance TermLike RDF.Node where
  varOrTerm n@(RDF.UNode _) = Term . IRIRefTerm . AbsoluteIRI $ n
  varOrTerm (RDF.LNode lv) = Term . RDFLiteralTerm $ lv
  varOrTerm (RDF.BNode i) = Term . BNode . Just $ i
  varOrTerm (RDF.BNodeGen i) = Term . BNode . Just . T.pack . mconcat $ ["genid", show i]

instance TermLike VarOrNode where
  varOrTerm (Var' v) = Var v
  varOrTerm (RDFNode n) = varOrTerm n

-- | Restriction of TermLike to the role of subject.
class (TermLike a) => SubjectTermLike a

instance SubjectTermLike IRIRef

instance SubjectTermLike Variable

instance SubjectTermLike EmbeddedTriple

instance SubjectTermLike BlankNodePattern

-- | Restriction of TermLike to the role of predicate.
class (TermLike a) => PredicateTermLike a

instance PredicateTermLike IRIRef

instance PredicateTermLike Variable

instance PredicateTermLike PropertyPathExpr

-- | Restriction of TermLike to the role of object.
class (TermLike a) => ObjectTermLike a

instance ObjectTermLike IRIRef

instance ObjectTermLike Variable

instance ObjectTermLike EmbeddedTriple

instance ObjectTermLike BlankNodePattern

instance ObjectTermLike Expr

instance ObjectTermLike Integer

instance ObjectTermLike T.Text

instance ObjectTermLike (T.Text, T.Text)

instance ObjectTermLike (T.Text, IRIRef)

instance ObjectTermLike Bool

instance ObjectTermLike VarOrNode

instance ObjectTermLike RDF.Node

-- Operations
operation :: (TermLike a, TermLike b) => Operation -> a -> b -> Expr
operation op x y = NumericExpr $ OperationExpr op (expr x) (expr y)

-- | Add two terms.
(.+.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.+.) = operation Add

-- | Find the difference between two terms.
(.-.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.-.) = operation Subtract

-- | Multiply two terms.
(.*.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.*.) = operation Multiply

-- | Divide two terms.
(./.) :: (TermLike a, TermLike b) => a -> b -> Expr
(./.) = operation Divide

-- | Combine two boolean terms with AND
(.&&.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.&&.) = operation And

-- | Combine two boolean terms with OR
(.||.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.||.) = operation Or

infixr 2 .||.

infixr 3 .&&.

infixl 7 .*.

infixl 7 ./.

infixl 6 .+.

infixl 6 .-.

-- Relations
relation :: (TermLike a, TermLike b) => Relation -> a -> b -> Expr
relation rel x y = RelationalExpr rel (expr x) (expr y)

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their equivalence.
(.==.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.==.) = relation Equal

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their equivalence.
(.!=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.!=.) = relation NotEqual

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their relative value.
(.<.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.<.) = relation LessThan

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their relative value.
(.>.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.>.) = relation GreaterThan

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their relative value.
(.<=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.<=.) = relation LessThanOrEqual

-- | Create an expression which tests the relationship of the two operands,
--   evaluating their relative value.
(.>=.) :: (TermLike a, TermLike b) => a -> b -> Expr
(.>=.) = relation GreaterThanOrEqual

infix 4 .==., .!=., .<., .<=., .>., .>=.

-- Negation

-- | Negate any term-like expression, for use, e.g., in filtering.
notExpr :: (TermLike a) => a -> Expr
notExpr = NegatedExpr . expr

-- Builtin Functions
type BuiltinFunc0 = Expr

builtinFunc0 :: Function -> BuiltinFunc0
builtinFunc0 f = BuiltinCall f []

type BuiltinFunc1 = forall a. (TermLike a) => a -> Expr

builtinFunc1 :: Function -> BuiltinFunc1
builtinFunc1 f x = BuiltinCall f [expr x]

type BuiltinFunc2 = forall a b. (TermLike a, TermLike b) => a -> b -> Expr

builtinFunc2 :: Function -> BuiltinFunc2
builtinFunc2 f x y = BuiltinCall f [expr x, expr y]

type BuiltinFunc3 = forall a b c. (TermLike a, TermLike b, TermLike c) => a -> b -> c -> Expr

builtinFunc3 :: Function -> BuiltinFunc3
builtinFunc3 f x y z = BuiltinCall f [expr x, expr y, expr z]

-- | Aggregate by count
count :: BuiltinFunc1
count = builtinFunc1 CountFunc

-- | Aggregate by sum
sum_ :: BuiltinFunc1
sum_ = builtinFunc1 SumFunc

-- | Aggregate by minimum value
min_ :: BuiltinFunc1
min_ = builtinFunc1 MinFunc

-- | Aggregate by maximum value
max_ :: BuiltinFunc1
max_ = builtinFunc1 MaxFunc

-- | Aggregate by average
avg :: BuiltinFunc1
avg = builtinFunc1 AvgFunc

-- | Cast as a string
str :: BuiltinFunc1
str = builtinFunc1 StrFunc

-- | Get the language of this element
lang :: BuiltinFunc1
lang = builtinFunc1 LangFunc

-- | strlen ( string ) - get the length of a string
strlen :: BuiltinFunc1
strlen = builtinFunc1 StrLenFunc

-- | substr ( string beginPosition stringLength ) - get a substring
substr :: BuiltinFunc1
substr = builtinFunc1 SubStrFunc

-- | ucase ( string ) - convert to upper case
ucase :: BuiltinFunc1
ucase = builtinFunc1 UcaseFunc

-- | lcase ( string ) - convert to lower case
lcase :: BuiltinFunc1
lcase = builtinFunc1 LcaseFunc

-- | strstarts ( string x ) - return true if x matches the beginning of string
strstarts :: BuiltinFunc2
strstarts = builtinFunc2 StrStartsFunc

-- | strends ( string x ) - return true if x matches the end of string
strends :: BuiltinFunc2
strends = builtinFunc2 StrEndsFunc

-- | contains ( string x ) - return true if x matches anywhere in string
contains :: BuiltinFunc2
contains = builtinFunc2 ContainsFunc

containsWith :: BuiltinFunc2
containsWith = builtinFunc2 ContainsWithFunc

-- | strbefore ( string x ) - return the string preceding a match to x
strbefore :: BuiltinFunc2
strbefore = builtinFunc2 StrBeforeFunc

-- | strafter ( string x ) - return the string after a match to x
strafter :: BuiltinFunc2
strafter = builtinFunc2 StrAfterFunc

-- | concat_ ( x y ) - concatenate strings x and y
concat_ :: BuiltinFunc2
concat_ = builtinFunc2 ConcatFunc

-- | replace ( string p r ) - replace literal p with literal r in string
replace :: BuiltinFunc3
replace = builtinFunc3 ReplaceFunc

-- | abs_ ( x ) - take the absolute value of number x
abs_ :: BuiltinFunc1
abs_ = builtinFunc1 AbsFunc

-- | round ( x ) - round x to the nearest integer
round_ :: BuiltinFunc1
round_ = builtinFunc1 RoundFunc

-- | ceil ( number ) - round x up to the nearest integer
ceil :: BuiltinFunc1
ceil = builtinFunc1 CeilFunc

-- | floor ( number ) - round x down to the nearest integer
floor_ :: BuiltinFunc1
floor_ = builtinFunc1 FloorFunc

-- | rand ( ) - produce a random number between 0 and 1
rand :: BuiltinFunc0
rand = builtinFunc0 RandFunc

-- | Aggregate a column by string concatenation with a separator.
groupConcat :: (TermLike a) => a -> String -> Expr
groupConcat x sep = ParameterizedCall GroupConcat [expr x] [("separator", "\"" ++ sep ++ "\"")]

langMatches :: BuiltinFunc2
langMatches = builtinFunc2 LangMatchesFunc

datatype :: BuiltinFunc1
datatype = builtinFunc1 DataTypeFunc

bound :: Variable -> Expr
bound x = BuiltinCall BoundFunc [expr x]

sameTerm :: BuiltinFunc2
sameTerm = builtinFunc2 SameTermFunc

isIRI :: BuiltinFunc1
isIRI = builtinFunc1 IsIRIFunc

isURI :: BuiltinFunc1
isURI = builtinFunc1 IsURIFunc

isBlank :: BuiltinFunc1
isBlank = builtinFunc1 IsBlankFunc

isLiteral :: BuiltinFunc1
isLiteral = builtinFunc1 IsLiteralFunc

regex :: BuiltinFunc2
regex = builtinFunc2 RegexFunc

regexOpts :: BuiltinFunc3
regexOpts = builtinFunc3 RegexFunc

-- Default QueryData
queryData :: QueryData
queryData =
  QueryData
    { prefixes = [],
      varsIdx = 0,
      vars = [],
      queryType = TypeNotSet,
      subQueryIdx = [0],
      pattern = GroupGraphPattern [],
      constructTriples = [],
      askTriples = [],
      updateTriples = [],
      describeURI = Nothing,
      duplicates = NoLimits,
      groups = [],
      havings = Nothing,
      ordering = [],
      limits = NoLimit
    }

-- Query representation
class QueryShow a where
  -- | Convert most query-related types to a 'String', most importantly
  --   'QueryData's.
  qshow :: a -> String

data Duplicates = NoLimits | Distinct | Reduced
  deriving (Show)

data Limit = NoLimit | Limit Int
  deriving (Show)

data Prefix = Prefix T.Text RDF.Node
  deriving (Show, Eq)

data Variable = Variable (NonEmpty Int)
  deriving (Show)

data EmbeddedTriple = EmbeddedTriple VarOrTerm
  deriving (Show)

data DynamicPredicate = forall a. (PredicateTermLike a, QueryShow a, Show a) => DynamicPredicate a

data DynamicObject = forall a. (ObjectTermLike a, QueryShow a, Show a) => DynamicObject a

type DynamicPredicateObject = (DynamicPredicate, DynamicObject)

type BlankNodePattern = [DynamicPredicateObject]

instance Show DynamicPredicate where
  show (DynamicPredicate x) = show x

instance Show DynamicObject where
  show (DynamicObject x) = show x

-- | support for blank nodes.
--
--  Define a convenient alias for `mkPredicateObject`. Note: a
--  pointfree definition leads to the monomorphism restriction @(&) =
--  mkPredicateObject@. An example of its use:
--
--  > p & o = mkPredicateObject p o
--
--  for example
--
--  > q = do
--  >   p <- prefix "" (iriRef "http://example.com/")
--  >   s <- var
--  >   o1 <- var
--  >   o2 <- var
--  >   _ <- triple s (p .:. "p1") [(p .:. "p2") & [(p .:. "p3") & o1], (p .:. "p4") & o2]
--  >   return SelectQuery { queryVars = [s, o1] }
--
--  >>> createSelectQuery q
--  "PREFIX : <http://example.com/> SELECT  ?x0 ?x1 WHERE {?x0 :p1 [:p2 [:p3 ?x1]], [:p4 ?x2] .} "
mkPredicateObject :: (PredicateTermLike a, ObjectTermLike b, QueryShow a, QueryShow b, Show a, Show b) => a -> b -> DynamicPredicateObject
mkPredicateObject p o = (DynamicPredicate p, DynamicObject o)

data IRIRef
  = AbsoluteIRI RDF.Node
  | PrefixedName Prefix T.Text
  deriving (Show)

iriRef :: T.Text -> IRIRef
iriRef uri = AbsoluteIRI $ RDF.unode uri

getFQN :: IRIRef -> T.Text
getFQN (AbsoluteIRI (RDF.UNode n)) = n
getFQN (PrefixedName (Prefix _ (RDF.UNode n)) s) = T.append n s
-- FIXME
getFQN _ = error "getFQN: input not supported"

-- FIXME: Should support numeric literals, too
data GraphTerm
  = IRIRefTerm IRIRef
  | RDFLiteralTerm RDF.LValue
  | NumericLiteralTerm Integer
  | BooleanLiteralTerm Bool
  | BNode (Maybe T.Text)
  | PropertyPathTerm PropertyPathExpr
  deriving (Show)

data VarOrTerm
  = Var Variable
  | Term GraphTerm
  | EmbeddedTriple' VarOrTerm VarOrTerm VarOrTerm
  | BlankNodePattern' BlankNodePattern
  deriving (Show)

-- | Enables programmatic construction of triples where it is not known in
--  advance which parts of the triple will be variables and which will be
--  'Node's.
data VarOrNode
  = Var' Variable
  | RDFNode RDF.Node
  deriving (Show)

data Operation = Add | Subtract | Multiply | Divide | And | Or
  deriving (Show)

data NumericExpr
  = NumericLiteralExpr Integer
  | OperationExpr Operation Expr Expr
  deriving (Show)

data Relation = Equal | NotEqual | LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual
  deriving (Show)

data Function
  = CountFunc
  | SumFunc
  | MinFunc
  | MaxFunc
  | AvgFunc
  | StrFunc
  | LangFunc
  | LangMatchesFunc
  | DataTypeFunc
  | BoundFunc
  | SameTermFunc
  | IsIRIFunc
  | IsURIFunc
  | IsBlankFunc
  | IsLiteralFunc
  | RegexFunc
  | StrLenFunc
  | SubStrFunc
  | UcaseFunc
  | LcaseFunc
  | StrStartsFunc
  | StrEndsFunc
  | ContainsFunc
  | ContainsWithFunc
  | StrBeforeFunc
  | StrAfterFunc
  | ConcatFunc
  | ReplaceFunc
  | AbsFunc
  | RoundFunc
  | CeilFunc
  | FloorFunc
  | RandFunc
  deriving (Show)

data ParameterizedFunction = GroupConcat deriving (Show)

data Expr
  = OrExpr [Expr]
  | AndExpr [Expr]
  | NegatedExpr Expr
  | RelationalExpr Relation Expr Expr
  | NumericExpr NumericExpr
  | BuiltinCall Function [Expr]
  | VarOrTermExpr VarOrTerm
  | ParameterizedCall ParameterizedFunction [Expr] [(String, String)]
  deriving (Show)

data SelectExpr
  = SelectExpr Expr Variable
  | SelectVar Variable
  deriving (Show)

as :: Expr -> Variable -> SelectExpr
e `as` v = SelectExpr e v

data Pattern
  = QTriple VarOrTerm VarOrTerm VarOrTerm
  | Filter Expr
  | FilterExistsPattern GroupGraphPattern
  | FilterNotExistsPattern GroupGraphPattern
  | Bind Expr Variable
  | OptionalGraphPattern GroupGraphPattern
  | ServiceGraphPattern IRIRef GroupGraphPattern
  | UnionGraphPattern GroupGraphPattern GroupGraphPattern
  | SubQuery QueryData
  deriving (Show)

data GroupGraphPattern = GroupGraphPattern [Pattern]
  deriving (Show)

newtype GroupBy = GroupBy Expr
  deriving (Show)

newtype Having = Having Expr
  deriving (Show)

data OrderBy
  = Asc Expr
  | Desc Expr
  deriving (Show)

-- Auxiliary, but fairly useful
-- TODO don't add to end
appendPattern :: Pattern -> GroupGraphPattern -> GroupGraphPattern
appendPattern p (GroupGraphPattern ps) = GroupGraphPattern (ps ++ [p])

appendTriple :: a -> [a] -> [a]
appendTriple t ts = t : ts

data QueryData = QueryData
  { prefixes :: [Prefix],
    varsIdx :: Int,
    vars :: [SelectExpr],
    queryType :: QueryType,
    subQueryIdx :: NonEmpty Int,
    pattern :: GroupGraphPattern,
    constructTriples :: [Pattern], -- QTriple
    askTriples :: [Pattern],
    updateTriples :: [Pattern],
    describeURI :: Maybe IRIRef,
    duplicates :: Duplicates,
    groups :: [GroupBy],
    havings :: Maybe Having,
    ordering :: [OrderBy],
    limits :: Limit
  }
  deriving (Show)

data QueryType = SelectType | ConstructType | AskType | UpdateType | DescribeType | TypeNotSet
  deriving (Show)

data ConstructQuery = ConstructQuery
  {queryConstructs :: [Pattern]}

data AskQuery = AskQuery
  {queryAsk :: [Pattern]}

data UpdateQuery = UpdateQuery
  {queryUpdate :: [Pattern]}

data SelectQuery = SelectQuery
  {queryExpr :: [SelectExpr]}

data DescribeQuery = DescribeQuery
  {queryDescribe :: IRIRef}

-- QueryShow instances
instance QueryShow BlankNodePattern where
  qshow [] = "[]"
  qshow xs = intercalate ", " $ fmap qshow xs

instance QueryShow DynamicPredicateObject where
  qshow (DynamicPredicate p, DynamicObject o) = mconcat ["[", qshow p, " ", qshow o, "]"]

instance QueryShow Duplicates where
  qshow NoLimits = ""
  qshow Distinct = "DISTINCT"
  qshow Reduced = "REDUCED"

instance QueryShow Limit where
  qshow NoLimit = ""
  qshow (Limit n) = "Limit " ++ show n

instance QueryShow RDF.Node where
  qshow (RDF.UNode n) = "<" ++ T.unpack n ++ ">"
  qshow (RDF.BNode n) = "_:" ++ T.unpack n
  qshow (RDF.BNodeGen i) = "_:genid" ++ show i
  qshow (RDF.LNode n) = qshow n

instance QueryShow RDF.LValue where
  qshow (RDF.PlainL lit) = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\""]
  qshow (RDF.PlainLL lit lang_) = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\"@", lang_]
  qshow (RDF.TypedL lit dtype) = T.unpack . T.concat $ ["\"", escapeSpecialChar lit, "\"^^<", dtype, ">"]

instance QueryShow Prefix where
  qshow (Prefix pre ref) = "PREFIX " ++ (T.unpack pre) ++ ": " ++ qshow ref

instance QueryShow [Prefix] where
  qshow = unwords . reverse . fmap qshow

instance QueryShow Variable where
  qshow (Variable vs) = "?x" ++ indexes
    where
      indexes = mconcat . intersperse "_" . fmap show . NE.toList $ vs

instance QueryShow [Variable] where
  qshow = unwords . fmap qshow

instance QueryShow IRIRef where
  qshow (AbsoluteIRI n) = qshow n
  qshow (PrefixedName (Prefix pre _) s) = (T.unpack pre) ++ ":" ++ (escape $ T.unpack s)
    where
      escape = URI.escapeURIString URI.isUnescapedInURIComponent

instance QueryShow (Maybe IRIRef) where
  qshow (Just r) = qshow r
  qshow Nothing = ""

instance QueryShow GraphTerm where
  qshow (IRIRefTerm ref) = qshow ref
  qshow (RDFLiteralTerm s) = qshow s
  qshow (BooleanLiteralTerm True) = show ("true" :: String)
  qshow (BooleanLiteralTerm False) = show ("false" :: String)
  qshow (NumericLiteralTerm i) = show i
  qshow (BNode Nothing) = "[]"
  qshow (BNode (Just i)) = "_:" ++ T.unpack i
  qshow (PropertyPathTerm p) = qshow p

instance QueryShow VarOrTerm where
  qshow (Var v) = qshow v
  qshow (Term t) = qshow t
  qshow (EmbeddedTriple' x y z) = intercalate " " ["<<", qshow x, qshow y, qshow z, ">>"]
  qshow (BlankNodePattern' bn) = qshow bn

instance QueryShow [VarOrTerm] where
  qshow = unwords . fmap qshow

instance QueryShow Operation where
  qshow Add = "+"
  qshow Subtract = "-"
  qshow Multiply = "*"
  qshow Divide = "/"
  qshow And = "&&"
  qshow Or = "||"

instance QueryShow NumericExpr where
  qshow (NumericLiteralExpr n) = show n
  qshow (OperationExpr op x y) = qshow x ++ qshow op ++ qshow y

instance QueryShow Relation where
  qshow Equal = "="
  qshow NotEqual = "!="
  qshow LessThan = "<"
  qshow GreaterThan = ">"
  qshow LessThanOrEqual = "<="
  qshow GreaterThanOrEqual = ">="

instance QueryShow Function where
  qshow CountFunc = "COUNT"
  qshow SumFunc = "SUM"
  qshow MinFunc = "MIN"
  qshow MaxFunc = "MAX"
  qshow AvgFunc = "AVG"
  qshow StrFunc = "STR"
  qshow LangFunc = "LANG"
  qshow LangMatchesFunc = "LANGMATCHES"
  qshow DataTypeFunc = "DATATYPE"
  qshow BoundFunc = "BOUND"
  qshow SameTermFunc = "sameTerm"
  qshow IsIRIFunc = "isIRI"
  qshow IsURIFunc = "isURI"
  qshow IsBlankFunc = "isBlank"
  qshow IsLiteralFunc = "isLiteral"
  qshow RegexFunc = "REGEX"
  qshow StrLenFunc = "STRLEN"
  qshow SubStrFunc = "SUBSTR"
  qshow UcaseFunc = "UCASE"
  qshow LcaseFunc = "LCASE"
  qshow StrStartsFunc = "STRSTARTS"
  qshow StrEndsFunc = "STARTENDS"
  qshow ContainsFunc = "CONTAINS"
  qshow ContainsWithFunc = "bif:contains"
  qshow StrBeforeFunc = "STRBEFORE"
  qshow StrAfterFunc = "STRAFTER"
  qshow ConcatFunc = "CONCAT"
  qshow ReplaceFunc = "REPLACE"
  qshow AbsFunc = "ABS"
  qshow RoundFunc = "ROUND"
  qshow CeilFunc = "CEIL"
  qshow FloorFunc = "FLOOR"
  qshow RandFunc = "RAND"

instance QueryShow ParameterizedFunction where
  qshow GroupConcat = "GROUP_CONCAT"

instance QueryShow Expr where
  qshow = qshow'
    where
      qshow' (VarOrTermExpr vt) = qshow vt
      qshow' (OrExpr es) = wrap $ intercalate " || " $ map qshow es
      qshow' (AndExpr es) = wrap $ intercalate " && " $ map qshow es
      qshow' (NegatedExpr e') = wrap $ '!' : qshow e'
      qshow' (RelationalExpr rel e1 e2) = wrap $ qshow e1 ++ qshow rel ++ qshow e2
      qshow' (NumericExpr e') = wrap $ qshow e'
      qshow' (BuiltinCall f es) = wrap $ qshow f ++ "(" ++ intercalate ", " (map qshow es) ++ ")"
      qshow' (ParameterizedCall f es kwargs) = wrap $ qshow f ++ "(" ++ intercalate ", " (map qshow es) ++ " ; " ++ (intercalate "," $ map pair kwargs) ++ ")"
      wrap e = "(" ++ e ++ ")"
      pair (k, v) = k ++ "=" ++ v

instance QueryShow SelectExpr where
  qshow (SelectVar v) = qshow v
  qshow (SelectExpr e v) = mconcat ["(", qshow e, " AS ", qshow v, ")"]

instance QueryShow [SelectExpr] where
  qshow = intercalate " " . fmap qshow

instance QueryShow Pattern where
  qshow (QTriple x y z) = intercalate " " [qshow x, qshow y, qshow z, "."]
  qshow (Filter e) = "FILTER " ++ qshow e ++ " ."
  qshow (FilterExistsPattern p) = "FILTER EXISTS " ++ qshow p
  qshow (FilterNotExistsPattern p) = "FILTER NOT EXISTS " ++ qshow p
  qshow (Bind e v) = "BIND(" ++ qshow e ++ " AS " ++ qshow v ++ ")"
  qshow (OptionalGraphPattern p) = "OPTIONAL " ++ qshow p
  qshow (ServiceGraphPattern endpoint p) = "SERVICE " ++ qshow endpoint ++ " " ++ qshow p
  qshow (UnionGraphPattern p1 p2) = qshow p1 ++ " UNION " ++ qshow p2
  qshow (SubQuery qd) = intercalate " " ["{ ", qshow qd, " }"]

instance QueryShow [Pattern] where
  qshow = unwords . fmap qshow

instance QueryShow GroupGraphPattern where
  qshow (GroupGraphPattern ps) = "{ " ++ qshow ps ++ " }"

instance QueryShow GroupBy where
  qshow (GroupBy e) = qshow e

instance QueryShow [GroupBy] where
  qshow [] = ""
  qshow gs = unwords $ "GROUP BY" : fmap qshow gs

instance QueryShow Having where
  qshow (Having e) = "HAVING (" ++ qshow e ++ ")"

instance QueryShow (Maybe Having) where
  qshow Nothing = ""
  qshow (Just e) = qshow e

instance QueryShow OrderBy where
  qshow (Asc e) = "ASC(" ++ qshow e ++ ")"
  qshow (Desc e) = "DESC(" ++ qshow e ++ ")"

instance QueryShow [OrderBy] where
  qshow [] = ""
  qshow os = unwords $ "ORDER BY" : fmap qshow os

instance QueryShow QueryData where
  qshow qd = query
    where
      prefixDecl = qshow (prefixes qd)
      whereClause = unwords ["WHERE", qshow (pattern qd)]
      groupClause = qshow . groups $ qd
      havingClause = qshow . havings $ qd
      orderClause = qshow . ordering $ qd
      -- TODO: Offset
      limitOffsetClauses = qshow (limits qd)
      solutionModifier = unwords' [groupClause, havingClause, orderClause, limitOffsetClauses]
      query = case queryType qd of
        SelectType ->
          unwords'
            [ prefixDecl,
              "SELECT",
              qshow (duplicates qd),
              qshow (vars qd),
              whereClause,
              solutionModifier
            ]
        ConstructType ->
          unwords
            [ prefixDecl,
              "CONSTRUCT {",
              qshow (constructTriples qd),
              "}",
              whereClause
            ]
        DescribeType ->
          unwords
            [ prefixDecl,
              "DESCRIBE",
              qshow (describeURI qd),
              whereClause
            ]
        AskType ->
          unwords
            [ prefixDecl,
              "ASK {",
              qshow (askTriples qd),
              "}"
            ]
        UpdateType ->
          unwords
            [ prefixDecl,
              "INSERT DATA {",
              qshow (updateTriples qd),
              "}"
            ]
        -- FIXME
        TypeNotSet ->
          error "instance QueryShow QueryData: TypeNotSet not supported."

-- Internal utilities
escapeSpecialChar :: T.Text -> T.Text
escapeSpecialChar = T.concatMap handleChar
  where
    -- FIXME: probably more cases to handle
    handleChar '\n' = "\n"
    handleChar '\t' = "\t"
    handleChar '\r' = "\r"
    handleChar '"' = "\\\""
    handleChar '\\' = "\\\\"
    handleChar c = T.singleton c

-- | Alternative version of 'unwords' that avoid adding spaces on empty strings.
{-# NOINLINE [1] unwords' #-}
unwords' :: [String] -> String
unwords' [] = ""
unwords' ("" : ws) = unwords' ws
unwords' (w : ws) = w ++ go ws
  where
    go [] = ""
    go ("" : vs) = go vs
    go (v : vs) = ' ' : (v ++ go vs)

-- | Append a element to a 'NonEmpty' list.
(|>) :: NonEmpty a -> a -> NonEmpty a
xs |> x = NE.fromList $ NE.toList xs ++ [x]

-- Property paths

-- | Permit different type of property path expressions to be seemlessly be put
--  into arguments for '(.//.)' and similar functions.
class PropertyPathExprLike a where
  propertyPathExpr :: a -> PropertyPathExpr

-- | Possible expressions in a SPARQL property path.
data PropertyPathExpr
  = -- | Property path of length one expression. Ex. @rdf:type@
    PathLengthOneExpr PathLengthOne
  | -- | Ex. @foaf:knows/foaf:knows@
    SequencePathExpr PropertyPathExpr PropertyPathExpr
  | -- | Ex. @^foaf:knows@
    InversePathExpr PropertyPathExpr
  | -- | Ex. @rdfs:label|foaf:name@
    AlternativePathExpr PropertyPathExpr PropertyPathExpr
  | -- | Ex. @rdf:type/rdfs:subClassOf*@
    ZeroOrMorePathExpr PropertyPathExpr
  | -- | Ex. @foaf:knows+@
    OneOrMorePathExpr PropertyPathExpr
  | -- | Ex. @rdf:type/rdfs:subClassOf?@
    ZeroOrOnePathExpr PropertyPathExpr
  | -- | Ex. @!(rdf:type|^rdf:type)@
    NegativePropertySetExpr NegativePropertySet
  deriving (Show)

-- instance PredicateTermLike PropertyPathExpr

-- | Possible expressions in a negated property set.
data NegativePropertySet
  = NegativePropertySetPath PathLengthOne NegativePropertySet
  | NegativePropertySetPathOne PathLengthOne
  deriving (Show)

instance QueryShow NegativePropertySet where
  qshow (NegativePropertySetPath p ps) = qshow p ++ "|" ++ qshow ps
  qshow (NegativePropertySetPathOne p) = qshow p

instance Semigroup NegativePropertySet where
  (NegativePropertySetPathOne p1) <> p2 = NegativePropertySetPath p1 p2
  (NegativePropertySetPath p1 ps) <> p2 = NegativePropertySetPath p1 (ps <> p2)

instance PropertyPathExprLike PropertyPathExpr where
  propertyPathExpr = id

-- newtype IRIRef = IRIRef Text

-- instance Show IRIRef where
--   show (IRIRef iri) = unpack iri

instance PropertyPathExprLike IRIRef where
  propertyPathExpr = PathLengthOneExpr . PathLengthOneIRI

data PathLengthOne
  = PathLengthOneIRI IRIRef
  | PathLengthOneA
  | PathLengthOneInverse PathLengthOne
  deriving (Show)

instance PropertyPathExprLike PathLengthOne where
  propertyPathExpr p = PathLengthOneExpr p

instance NegativePropertySetLike PathLengthOne where
  negativePropertySet p = NegativePropertySetPathOne p

instance QueryShow PropertyPathExpr where
  qshow (PathLengthOneExpr p) = qshow p
  qshow (SequencePathExpr pexpr1 pexpr2) =
    "(" ++ qshow pexpr1 ++ "/" ++ qshow pexpr2 ++ ")"
  qshow (InversePathExpr pexpr) = "(^" ++ qshow pexpr ++ ")"
  qshow (AlternativePathExpr pexpr1 pexpr2) =
    "(" ++ qshow pexpr1 ++ "|" ++ qshow pexpr2 ++ ")"
  qshow (ZeroOrMorePathExpr pexpr) = "(" ++ qshow pexpr ++ "*)"
  qshow (OneOrMorePathExpr pexpr) = "(" ++ qshow pexpr ++ "+)"
  qshow (ZeroOrOnePathExpr pexpr) = "(" ++ qshow pexpr ++ "?)"
  qshow (NegativePropertySetExpr p) = "!(" ++ qshow p ++ ")"

instance QueryShow PathLengthOne where
  qshow (PathLengthOneIRI iri) = qshow iri
  qshow PathLengthOneA = "a"
  qshow (PathLengthOneInverse p) = "^" ++ qshow p

class NegativePropertySetLike a where
  negativePropertySet :: a -> NegativePropertySet

instance NegativePropertySetLike IRIRef where
  negativePropertySet iri = NegativePropertySetPathOne $ PathLengthOneIRI iri

instance NegativePropertySetLike NegativePropertySet where
  negativePropertySet ps = ps

-- | Creating a property path sequence.
--
--  >>> IRIRef "rdf:type" ./. IRIRef "rdfs:subClassOf" ./. IRIRef "rdfs:subClassOf"
--  rdf:type/rdfs:subClassOf/rdfs:subClassOf
infixl 5 .//.

(.//.) ::
  (PropertyPathExprLike a, PropertyPathExprLike b) =>
  a ->
  b ->
  PropertyPathExpr
x .//. y = SequencePathExpr (propertyPathExpr x) (propertyPathExpr y)

-- | Creating an alternative property path.
--
--  >>> IRIRef "rdfs:label" .|. IRIRef "foaf:name" .|. IRIRef "foaf:givenName
--  rdfs:label|foaf:name|foaf:givenName
infixl 4 .|.

(.|.) ::
  (PropertyPathExprLike a, PropertyPathExprLike b) =>
  a ->
  b ->
  PropertyPathExpr
x .|. y = AlternativePathExpr (propertyPathExpr x) (propertyPathExpr y)

-- | Creating an alternative property path inside a negative property set.
--
--  >>> neg $ IRIRef "rdfs:label" ..|.. IRIRef "foaf:name" ..|.. IRIRef "foaf:givenName"
--  !(rdfs:label|foaf:name|foaf:givenName)
infixl 4 ..|..

(..|..) ::
  (NegativePropertySetLike a, NegativePropertySetLike b) =>
  a ->
  b ->
  NegativePropertySet
p1 ..|.. p2 = negativePropertySet p1 <> negativePropertySet p2

-- | Creating an inverse property path.
--
--  >>> inv (IRIRef "foaf:mbox")
--  ^foaf:mbox
inv :: PropertyPathExprLike a => a -> PropertyPathExpr
inv p = InversePathExpr (propertyPathExpr p)

-- | Creating an inverse path of length one inside a negative property set.
--
--  >>> neg $ IRIRef "rdfs:label" ..|.. IRIRef "foaf:name" ..|.. (inv' $ IRIRef "foaf:givenName")
--  !(rdfs:label|foaf:name|^foaf:givenName)
inv' :: NegativePropertySetLike a => a -> NegativePropertySet
inv' p = case negativePropertySet p of
  NegativePropertySetPathOne p1 ->
    NegativePropertySetPathOne (PathLengthOneInverse p1)
  NegativePropertySetPath p1 ps ->
    NegativePropertySetPath (PathLengthOneInverse p1) ps

-- | Creating a negative property set.
--
--  >>> inv foafMbox
--  ^foaf:mbox
neg :: NegativePropertySet -> PropertyPathExpr
neg = NegativePropertySetExpr

-- | Variable "a" representing the @rdf:type@ property.
a :: PathLengthOne
a = PathLengthOneA

-- | Creating a zero or more path.
--
--  >>> IRIRef "rdf:type" ./. ((IRIRef "rdfs:subClassOf") *.)
--  rdf:type/rdfs:subClassOf*
(*.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(*.) p = ZeroOrMorePathExpr (propertyPathExpr p)

-- | Creating a one or more path.
--
--  >>> ((IRIRef "foaf:knows") +.) ./. IRIRef "foaf:name"
--  foaf:knows+/foaf:name
(+.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(+.) p = OneOrMorePathExpr (propertyPathExpr p)

-- | Creating a zero or one path.
--
--  >>> rdfType ./. (rdfsSubClassOf ?.)
--  rdf:type/rdfs:subClassOf?
(?.) :: PropertyPathExprLike a => a -> PropertyPathExpr
(?.) p = ZeroOrOnePathExpr (propertyPathExpr p)
