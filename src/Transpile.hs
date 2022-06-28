{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Transpile where

import L4.KeyValueMap
    ( ValueKVM(MapVM, IntVM, IdVM),
      selectOneOfInstr,
      getAssocOfPathValue )
import L4.Syntax
import L4.SyntaxManipulation (
      spine,
      ruleToFormula,
      conjsExpr,
      notExpr, etaExpand, decomposeFun, mkVarE)
import L4.Typing (isBooleanTp, isIntegerTp, isFloatTp, superClassesOfClassDecl)
import RuleTransfo ( isNamedRule )

import Control.Monad ( when, foldM )
import L4.PrintProg (renameExpr, printARName, ShowL4 (showL4) )
import Data.Maybe (fromMaybe)

import Control.Arrow ((>>>))
import Data.Function ((&))
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty ((:|)), toList, fromList)
import Data.Text (unpack, pack)
import GHC.Natural (naturalToInt)
import qualified Llvm as KindT

-------------------------------------------------------------
-- Data types stolen from repo dsl/ Types.hs
-------------------------------------------------------------


data ParamType = TOne | TOptional | TList0 | TList1
  deriving (Eq, Show)
type EntityType = Text.Text

data TypeSig = SimpleType ParamType EntityType
             | InlineEnum ParamType ParamText
             deriving (Eq, Show)
type KVsPair = (NonEmpty Text.Text, Maybe TypeSig)    --- so really there are multiple Values
type TypedMulti = KVsPair                             --- | apple | orange | banana | :: | Fruit   |

type MultiTerm = [Text.Text]

type ParamText = NonEmpty TypedMulti


data RelationalPredicate = RPParamText   ParamText                     -- cloudless blue sky
                         | RPMT MultiTerm -- intended to replace RPParamText. consider TypedMulti?
--                         | RPConstraint  MultiTerm RPRel MultiTerm     -- eyes IS blue
--                         | RPBoolStructR MultiTerm RPRel BoolStructR   -- eyes IS (left IS blue
--                                                                       --          AND
--                                                                       --          right IS brown)
  deriving (Eq, Show)


data Label a =
    Pre a
  | PrePost a a
  deriving (Eq, Show)

type Item a = Item' (Label Text.Text) a

data Item' lbl a =
    Leaf                       a
  | All (Maybe lbl) [Item' lbl a]
  | Any (Maybe lbl) [Item' lbl a]
  | Not             (Item' lbl a)
  deriving (Eq, Show)


type BoolStructR = Item RelationalPredicate


data HornClause2 = HC2
  { hHead :: RelationalPredicate
  , hBody :: Maybe BoolStructR
  }
  deriving (Eq, Show)


type RuleName   = MultiTerm

-- Etc. Still very incomplete
data MyToken = Every | Party | TokAll

type RuleLabel = (Text.Text   --  "§"
                 ,Int         --   1
                 ,Text.Text   --  "My First Rule"
                 )

data SrcRef = SrcRef { url      :: Text.Text
                     , short    :: Text.Text
                     , srcrow   :: Int
                     , srccol   :: Int
                     , version  :: Maybe Text.Text
                     }
              deriving (Eq, Show)


data Rule =   Hornlike
            { name     :: RuleName           -- MyInstance
            , super    :: Maybe TypeSig         -- IS A Superclass
            , keyword  :: MyToken            -- decide / define / means
            , given    :: Maybe ParamText    -- applicant has submitted fee
            , upon     :: Maybe ParamText    -- second request occurs
            , clauses  :: [HornClause2]      -- colour IS blue WHEN fee > $10 ; colour IS green WHEN fee > $20 AND approver IS happy
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }       
  | TypeDecl
            { name     :: RuleName              -- DECLARE Class
            , super    :: Maybe TypeSig         -- IS A Superclass
            , has      :: [Transpile.Rule]      -- HAS foo :: List Hand \n bar :: Optional Restaurant
            , enums    :: Maybe ParamText   -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , given    :: Maybe ParamText
            , upon     :: Maybe ParamText
            , rlabel   :: Maybe RuleLabel
            , lsource  :: Maybe Text.Text
            , srcref   :: Maybe SrcRef
            , defaults :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            , symtab   :: [RelationalPredicate] -- SomeConstant IS 500 ; MentalCapacity TYPICALLY True
            }

-------------------------------------------------------------
-- Auxiliary functions 
-------------------------------------------------------------
-- These are the untyped versions of typed functions defined in SyntaxManipulation.hs

-- compose (f, [a1 .. an]) to (f a1 .. an)
funArgsToAppUT :: Expr () -> [Expr ()] -> Expr ()
funArgsToAppUT = foldl (AppE ())

applyVarsUT :: Var () -> [Var ()] -> Expr ()
applyVarsUT f args = funArgsToAppUT (mkVarE f) (map mkVarE args)


trueVUT :: Expr ()
trueVUT = ValE () (BoolV True)
falseVUT :: Expr ()
falseVUT = ValE () (BoolV False)

notExprUT :: Expr () -> Expr ()
notExprUT = UnaOpE () (UBool UBnot)

conjExprUT :: Expr () -> Expr () -> Expr ()
conjExprUT = BinOpE () (BBool BBand)

disjExprUT :: Expr () -> Expr () -> Expr ()
disjExprUT = BinOpE () (BBool BBor)


conjsExprUT :: [Expr ()] -> Expr ()
conjsExprUT [] = trueVUT
conjsExprUT [e] = e
conjsExprUT (e:es) = conjExprUT e (conjsExprUT es)

disjsExprUT :: [Expr ()] -> Expr ()
disjsExprUT [] = falseVUT
disjsExprUT [e] = e
disjsExprUT (e:es) = disjExprUT e (disjsExprUT es)

-------------------------------------------------------------
-- Translation functions
-------------------------------------------------------------

textToVar :: Text.Text -> Var ()
textToVar t = GlobalVar (QVarName () (unpack t))

relationalPredicateToExpr :: RelationalPredicate -> Expr ()
relationalPredicateToExpr (RPMT (x:xs)) = applyVarsUT (textToVar x) (map textToVar xs)
relationalPredicateToExpr _ = trueVUT

boolStructRToExpr :: BoolStructR -> Expr ()
boolStructRToExpr (Leaf v) = relationalPredicateToExpr v
boolStructRToExpr (Transpile.All _lbls itms) = conjsExprUT (map boolStructRToExpr itms)
boolStructRToExpr (Transpile.Any _lbls itms) = disjsExprUT (map boolStructRToExpr itms)
boolStructRToExpr (Transpile.Not itm) = notExprUT (boolStructRToExpr itm)

hornClauseToRule :: HornClause2 -> L4.Syntax.Rule ()
hornClauseToRule (HC2 hd bd) =
      Rule
      { annotOfRule = ()
      , nameOfRule = Nothing
      , instrOfRule = []
      , varDeclsOfRule = []
      , precondOfRule = maybe trueVUT boolStructRToExpr bd
      , postcondOfRule = relationalPredicateToExpr hd
      }

{-
 - Helper function to convert names occuring in type declarations in natural l4
 - to baby l4.
 - Given a type a which is one of {ClassName, FieldName, VarName} and an
 - appropriate data constructor (toBabyName :: String -> a) of a, we return a
 - function of type (RuleName -> a).
 - This returned function uses toBabyName to construct a value of type a
 - out of the input rule name.
-}
naturalToBabyName :: (String -> a) -> RuleName -> a
naturalToBabyName toBabyName = head >>> unpack >>> toBabyName

ruleNameToClassName :: RuleName -> ClassName
ruleNameToClassName = naturalToBabyName ClsNm

ruleNameToFieldName :: RuleName -> FieldName
ruleNameToFieldName = naturalToBabyName FldNm

ruleNameToVarName :: RuleName -> VarName
ruleNameToVarName = naturalToBabyName id
-- Note here that we have the type equality (String ~ VarName)

superToClassDefSupers :: Maybe TypeSig -> [ClassName]
superToClassDefSupers Nothing = [ClassC]
superToClassDefSupers (Just (SimpleType TOne n)) = [ClsNm (unpack n)]
superToClassDefSupers _ = undefined


-- TODO: this function is a gross oversimplification as it only deals with atomic types 
-- and converts these to class types. 
-- Still to be done: correctly translate predefined types (Boolean, Integer ...)
-- and function types
superToType :: Maybe TypeSig -> Tp ()
superToType Nothing = BooleanT
superToType (Just ( SimpleType TOne t )) = ClassT () (ClsNm (unpack t))
superToType _ = undefined

typeDeclToFieldDecl :: Transpile.Rule -> FieldDecl ()
typeDeclToFieldDecl 
   TypeDecl { name   
            , super  
            , has    
            , enums    -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel
            , lsource
            , srcref
            } = FieldDecl {..}
            where
              annotOfFieldDecl = ()
              nameOfFieldDecl = ruleNameToFieldName name
              tpOfFieldDecl = superToType super
typeDeclToFieldDecl _ = undefined

typeDeclToClassDecl :: Transpile.Rule -> ClassDecl ()
typeDeclToClassDecl 
   TypeDecl { name   
            , super  
            , has    
            , enums    -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel
            , lsource
            , srcref
            } = ClassDecl {..}
            where
              annotOfClassDecl = ()
              nameOfClassDecl = ruleNameToClassName name
              defOfClassDecl = ClassDef supers fields
              supers = superToClassDefSupers super
              fields = map typeDeclToFieldDecl has
typeDeclToClassDecl _ = undefined

-- Example:
-- in natural4: DECLARE joe IS A Person
-- in BabyL4: decl joe : Person
varDeclToVarDecl :: Transpile.Rule -> VarDecl ()
varDeclToVarDecl 
   TypeDecl { name   
            , super  
            , has    
            , enums    -- ONE OF rock, paper, scissors (basically, disjoint subtypes)
            , rlabel
            , lsource
            , srcref
            } = VarDecl {..}
            where
              annotOfVarDecl = ()
              nameOfVarDecl = ruleNameToVarName name
              tpOfVarDecl = superToType super
              -- KindT is used as a dummy value. How do we determine what this is?
varDeclToVarDecl _ = undefined

-------------------------------------------------------------
-- Tests
-------------------------------------------------------------

hc1 :: HornClause2
hc1 = HC2
            { hHead = RPMT [ pack "d" ]
            , hBody = Just
                ( Transpile.All Nothing
                    [ Leaf
                        ( RPMT [ pack "a" ] )
                    , Any Nothing
                        [ Leaf
                            ( RPMT [ pack "b" ] )
                        , Leaf
                            ( RPMT [ pack "c" ] )
                        ]
                    ]
                )
            }

rl1 :: L4.Syntax.Rule ()
rl1 = hornClauseToRule hc1

{-
>>> showL4 [] rl1
rule 

if (a && (b || c))
then d

if (a && (b || c))
then d

if (a && (b || c))
then d

if (a && (b || c))
then d

-}

td1 :: Transpile.Rule
td1 = TypeDecl
    { name = [ pack "Corporation" ]
    , super = Just
        ( SimpleType TOne (pack "Party" ))
    , has =
        [ TypeDecl
            { name = ["representative" ]
            , super = Just
                ( SimpleType TOne "Natural Person" )
            , has = []
            , enums = Nothing
            , given = Nothing
            , upon = Nothing
            , rlabel = Nothing
            , lsource = Nothing
            , srcref = Nothing
            , defaults = []
            , symtab = []
            }
        ]
    , enums = Nothing
    , given = Nothing
    , upon = Nothing
    , rlabel = Nothing
    , lsource = Nothing
    , srcref = Just
        ( SrcRef
            { url = pack "ontology_defn.csv"
            , short = pack "ontology_defn.csv"
            , srcrow = 1
            , srccol = 5
            , version = Nothing
            }
        )
    , defaults = []
    , symtab = []
    }


cd1 :: ClassDecl ()
cd1 = typeDeclToClassDecl td1 


-- note: showL4 only works for correctly typed classes (with elaborated list of superclasses)
{-
>>> cd1
ClassDecl {annotOfClassDecl = (), nameOfClassDecl = ClsNm {stringOfClassName = "Corporation"}, defOfClassDecl = ClassDef {supersOfClassDef = [ClsNm {stringOfClassName = "Party"}], fieldsOfClassDef = [FieldDecl {annotOfFieldDecl = (), nameOfFieldDecl = FldNm {stringOfFieldName = "representative"}, tpOfFieldDecl = ClassT {annotOfTp = (), classNameOfTp = ClsNm {stringOfClassName = "Natural Person"}}}]}}

-}

{-
  Variable declarations in natural l4 can be expressed as:
        DECLARE myint IS A Integer
  This corresponds to the following declaration in baby l4:
        decl myint : Integer

  As of 2022-06-28, the natural l4 parser in the tab-mustsing branch of dsl
  parses such declarations into the AST below.
  This can be constructed by copying the included ontology_defn.csv file into
  the natural l4 directory and running
      stack run -- --only native ontology_defn.csv
      
  Running
    varDeclToVarDecl varDeclExamle
  in ghci yields:
    VarDecl {annotOfVarDecl = (), nameOfVarDecl = "myint", tpOfVarDecl = KindT}
-}
varDecl1 :: Transpile.Rule
varDecl1 =
  TypeDecl
    { name = ["myint"],
      super = Just (SimpleType TOne "Integer"),
      has = [],
      enums = Nothing,
      given = Nothing,
      upon = Nothing,
      rlabel = Nothing,
      lsource = Nothing,
      srcref =
        Just
          ( SrcRef
              { url = "ontology_defn.csv",
                short = "ontology_defn.csv",
                srcrow = 1,
                srccol = 34,
                version = Nothing
              }
          ),
      defaults = [],
      symtab = []
    }
    
{-
varDecl1' when evaluated in ghc has the following body:
  VarDecl1 {annotOfVarDecl = (), nameOfVarDecl = "myint", tpOfVarDecl = KindT}
-}
varDecl1' :: VarDecl ()
varDecl1' = varDeclToVarDecl varDecl1

{-
In natural l4, the following is expressed as
    DECLARE joe IS A Person
which corresponds to 
    decl joe : Person
in baby l4
 -}
varDecl2 :: Transpile.Rule
varDecl2 =
  TypeDecl
    { name = ["joe"],
      super = Just (SimpleType TOne "Person"),
      has = [],
      enums = Nothing,
      given = Nothing,
      upon = Nothing,
      rlabel = Nothing,
      lsource = Nothing,
      srcref =
        Just
          ( SrcRef
              { url = "ontology_defn.csv",
                short = "ontology_defn.csv",
                srcrow = 1,
                srccol = 35,
                version = Nothing
              }
          ),
      defaults = [],
      symtab = []
    }
    
{-
  ghci evaluates this to
    VarDecl {annotOfVarDecl = (), nameOfVarDecl = "joe", tpOfVarDecl = KindT}
  Note here that above, we hardcoded tpOfVarDecl to be KindT.
  However, the AST produced by running the baby l4 parser indicates that this
  should actually be ClassT.
-}
varDecl2' :: VarDecl ()
varDecl2' = varDeclToVarDecl varDecl2

{-
>>>  varDecl2'
VarDecl {annotOfVarDecl = (), nameOfVarDecl = "joe", tpOfVarDecl = ClassT {annotOfTp = (), classNameOfTp = ClsNm {stringOfClassName = "Person"}}}
-}
