
{-# LANGUAGE NamedFieldPuns #-}
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


import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty ((:|)), toList, fromList)
import Data.Text (unpack, pack)

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


ruleNameToClassName :: RuleName -> ClassName
ruleNameToClassName [n] = ClsNm (unpack n)
ruleNameToClassName _ = undefined


ruleNameToFieldName :: RuleName -> FieldName
ruleNameToFieldName [n] = FldNm (unpack n)
ruleNameToFieldName _ = undefined

superToClassDefSupers :: Maybe TypeSig -> [ClassName]
superToClassDefSupers Nothing = [ClassC]
superToClassDefSupers (Just (SimpleType TOne n)) = [ClsNm (unpack n)]
superToClassDefSupers _ = undefined

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
            } = FieldDecl () (ruleNameToFieldName name) (superToType super)
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
            } = 
              let supers = superToClassDefSupers super
                  fields = map typeDeclToFieldDecl has
                  cd = ClassDef supers fields
                  n = ruleNameToClassName name
              in ClassDecl () n cd
typeDeclToClassDecl _ = undefined

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

-}



td1 :: Transpile.Rule
td1 = TypeDecl
    { name = [ pack "Corporation" ]
    , super = Just
        ( SimpleType TOne (pack "Party" ))
    , has =
        [ TypeDecl
            { name = [pack "representative" ]
            , super = Just
                ( SimpleType TOne (pack "Natural Person" ))
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
