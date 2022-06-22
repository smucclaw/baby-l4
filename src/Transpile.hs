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

hornClauseToRule :: HornClause2 -> Rule ()
hornClauseToRule (HC2 hd bd) =
      Rule
      { annotOfRule = ()
      , nameOfRule = Nothing
      , instrOfRule = []
      , varDeclsOfRule = []
      , precondOfRule = maybe trueVUT boolStructRToExpr bd
      , postcondOfRule = relationalPredicateToExpr hd
      }


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

rl1 :: Rule ()
rl1 = hornClauseToRule hc1

{-
>>> showL4 [] rl1
rule 

if (a && (b || c))
then d

-}

