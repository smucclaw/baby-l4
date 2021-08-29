{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# OPTIONS_GHC -Wpartial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax where


-- Class for annotated expressions
--import qualified Language.LSP.Types as J    -- TODO: is that used anywhere?
--import qualified Data.List as List
import Data.Data (Data, Typeable)
import Annotation
import KeyValueMap
import PGF (Probabilities)


----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------


----- Names
type VarName = String
-- Assertion / Rule name 
type ARName = Maybe String
-- newtype VarName = VarName String
--   deriving (Eq, Ord, Show, Read, Data, Typeable)

newtype ClassName = ClsNm {stringOfClassName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype FieldName = FldNm {stringOfFieldName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype PartyName = PtNm {stringOfPartyName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- a qualified var name has an annotation (contrary to a simple var name)
data QVarName t = QVarName {annotOfQVarName :: t, nameOfQVarName :: VarName}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (QVarName t) where
  getLoc v = getLoc (annotOfQVarName v)

----- Program

data Program t = Program{ annotOfProgram :: t
                            , lexiconOfProgram :: [Mapping t]
                            , classDeclsOfProgram ::  [ClassDecl t]
                            , globalsOfProgram :: [VarDecl t]
                            , rulesOfProgram :: [Rule t]
                            , assertionsOfProgram :: [Assertion t] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

data TopLevelElement t
  = MappingTLE (Mapping t)
  | ClassDeclTLE (ClassDecl t)
  | VarDeclTLE (VarDecl t)
  | RuleTLE (Rule t)
  | AssertionTLE (Assertion t)
  | AutomatonTLE (TA t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

getAnnotOfTLE :: TopLevelElement t -> t
getAnnotOfTLE e = case e of
     MappingTLE mp -> annotOfMapping mp
     ClassDeclTLE cd -> annotOfClassDecl cd
     VarDeclTLE vd -> annotOfVarDecl vd
     RuleTLE ru -> annotOfRule ru
     AssertionTLE as -> annotOfAssertion as
     AutomatonTLE ta -> annotOfTA ta

updateAnnotOfTLE :: (t -> t) -> TopLevelElement t -> TopLevelElement t
updateAnnotOfTLE f e = case e of
     MappingTLE mp -> MappingTLE $ mp { annotOfMapping = f (annotOfMapping mp) }
     ClassDeclTLE cd -> ClassDeclTLE $ cd { annotOfClassDecl = f (annotOfClassDecl cd) }
     VarDeclTLE vd -> VarDeclTLE $ vd { annotOfVarDecl = f (annotOfVarDecl vd) }
     RuleTLE ru -> RuleTLE $ ru { annotOfRule = f (annotOfRule ru) }
     AssertionTLE as -> AssertionTLE $ as { annotOfAssertion = f (annotOfAssertion as) }
     AutomatonTLE ta -> AutomatonTLE $ ta { annotOfTA = f (annotOfTA ta) }
     
instance HasLoc t => HasLoc (TopLevelElement t) where
  getLoc = getLoc . getAnnotOfTLE

instance HasAnnot TopLevelElement where
  getAnnot = getAnnotOfTLE 
  updateAnnot = updateAnnotOfTLE

data NewProgram t = NewProgram { annotOfNewProgram :: t
                               , elementsOfNewProgram :: [TopLevelElement t] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasAnnot NewProgram where
  getAnnot = annotOfNewProgram
  updateAnnot f p = p { annotOfNewProgram = f (annotOfNewProgram p)}


isMapping :: TopLevelElement t -> Bool
isMapping (MappingTLE _) = True
isMapping _ = False

isClassDecl :: TopLevelElement t -> Bool
isClassDecl (ClassDeclTLE _) = True
isClassDecl _ = False

isVarDecl :: TopLevelElement t -> Bool
isVarDecl (VarDeclTLE _) = True
isVarDecl _ = False

isRule :: TopLevelElement t -> Bool
isRule (RuleTLE _) = True
isRule _ = False

isAssertion :: TopLevelElement t -> Bool
isAssertion (AssertionTLE _) = True
isAssertion _ = False

isAutomaton :: TopLevelElement t -> Bool
isAutomaton (AutomatonTLE _) = True
isAutomaton _ = False

getMapping :: TopLevelElement t -> Mapping t
getMapping (MappingTLE e) = e
getMapping _ = error "wrong selection"

getClassDecl :: TopLevelElement t -> ClassDecl t
getClassDecl (ClassDeclTLE e) = e
getClassDecl _ = error "wrong selection"

getVarDecl :: TopLevelElement t -> VarDecl t
getVarDecl (VarDeclTLE e) = e
getVarDecl _ = error "wrong selection"

getRule :: TopLevelElement t -> Rule t
getRule (RuleTLE e) = e
getRule _ = error "wrong selection"

getAssertion :: TopLevelElement t -> Assertion t
getAssertion (AssertionTLE e) = e
getAssertion _ = error "wrong selection"

getAutomaton :: TopLevelElement t -> TA t
getAutomaton (AutomatonTLE e) = e
getAutomaton _ = error "wrong selection"

lexiconOfNewProgram :: NewProgram t -> [Mapping t]
lexiconOfNewProgram = map getMapping . filter isMapping . elementsOfNewProgram

classDeclsOfNewProgram :: NewProgram t -> [ClassDecl t]
classDeclsOfNewProgram = map getClassDecl . filter isClassDecl . elementsOfNewProgram

globalsOfNewProgram :: NewProgram t -> [VarDecl t]
globalsOfNewProgram = map getVarDecl . filter isVarDecl . elementsOfNewProgram

rulesOfNewProgram :: NewProgram t -> [Rule t]
rulesOfNewProgram = map getRule . filter isRule . elementsOfNewProgram

assertionsOfNewProgram :: NewProgram t -> [Assertion t]
assertionsOfNewProgram = map getAssertion . filter isAssertion . elementsOfNewProgram

automataOfNewProgram :: NewProgram t -> [TA t]
automataOfNewProgram = map getAutomaton . filter isAutomaton . elementsOfNewProgram

mapClassDecl :: (ClassDecl t -> ClassDecl t)-> TopLevelElement t -> TopLevelElement t
mapClassDecl f e = case e of
  ClassDeclTLE cd -> ClassDeclTLE (f cd)
  x -> x

mapRule :: (Rule t -> Rule t) -> TopLevelElement t -> TopLevelElement t
mapRule f e = case e of
  RuleTLE r -> RuleTLE (f r)
  x -> x  

mapAssertion :: (Assertion t -> Assertion t) -> TopLevelElement t -> TopLevelElement t
mapAssertion f e = case e of
  AssertionTLE r -> AssertionTLE (f r)
  x -> x  

newProgramToProgram :: NewProgram t -> Program t
newProgramToProgram np = Program {
  annotOfProgram = annotOfNewProgram np,
  lexiconOfProgram = lexiconOfNewProgram np,
  classDeclsOfProgram = classDeclsOfNewProgram np,
  globalsOfProgram = globalsOfNewProgram np,
  rulesOfProgram = rulesOfNewProgram np,
  assertionsOfProgram = assertionsOfNewProgram np
}

instance HasAnnot Program where
  getAnnot = annotOfProgram
  updateAnnot f p = p { annotOfProgram = f (annotOfProgram p)}


data ExpectedType
  = ExpectedString  String
  | ExpectedExactTp (Tp())
  | ExpectedSubTpOf (Tp())
  deriving (Eq, Ord, Show, Read, Data, Typeable)


data ErrorCause
  = Inherited
  | UndefinedType [(SRng, ClassName)]            -- cf with the UndefinedType... constructors in Errors.hs
  | UndefinedTypeInClassT
  | UndeclaredVariable SRng VarName
  | IllTypedSubExpr { exprRangesITSE :: [SRng]    -- operators that require specific types  for their arguments
                    , receivedITSE :: [Tp()]
                    , expectedITSE :: [ExpectedType] }
  | IncompatibleTp { exprRangesITSE :: [SRng]     -- when we need two types to be the same for an operation, or perhaps a subtype (to check)
                    , receivedITSE :: [Tp()] }
  | NonScalarExpr { exprRangesITSE :: [SRng]      -- functions are not scalar types and not comparable
                    , receivedITSE :: [Tp()] }
  | NonFunctionTp { exprRangesITSE :: [SRng] -- call function when not function
                    , receivedFunTpITSE :: Tp() }
  | CastIncompatible { exprRangesITSE :: [SRng] -- typecasting from int to string for example (and its not compatible)
                    , receivedCastITSE :: Tp()
                    , castToITSE :: Tp() }
  | IncompatiblePattern SRng          -- pattern matching failure for tuples (l4)
  | UnknownFieldName SRng FieldName ClassName   -- class has no such field
  | AccessToNonObjectType SRng  -- when using dot notation on something thats not an object
  | Unspecified                 -- don't know, need clarification from martin?

  -- Previously spread out in several other types
  | DuplicateClassNamesCDEErr [(SRng, ClassName)]  -- classes whose name is defined multiple times
  | UndefinedSuperclassCDEErr [(SRng, ClassName)]  -- classes having undefined superclasses
  | CyclicClassHierarchyCDEErr [(SRng, ClassName)]  -- classes involved in a cyclic class definition

  | DuplicateFieldNamesFDEErr [(SRng, ClassName, [(SRng, FieldName)])]     -- field names with duplicate defs
  | UndefinedTypeFDEErr [(SRng, FieldName)]                                -- field names containing undefined types

  | DuplicateVarNamesVDEErr [(SRng, VarName)]
  | UndefinedTypeVDEErr [(SRng, VarName)]
  deriving (Eq, Ord, Show, Read, Data, Typeable)


----- Types
-- TODO: also types have to be annotated with position information
-- for the parser to do the right job
data Tp t
  = ClassT {annotOfTp :: t, classNameOfTp :: ClassName}
  | FunT {annotOfTp :: t, funTp :: Tp t, argTp :: Tp t}
  | TupleT {annotOfTp :: t, componentsOfTpTupleT :: [Tp t]}
  | ErrT {causeOfTpErrT :: ErrorCause}
  | ErrTWOCause
  | OkT        -- fake type appearing in constructs (classes, rules etc.) that do not have a genuine type
  | KindT
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (Tp t) where
  getLoc e = getLoc (annotOfTp e)

instance HasDefault (Tp t) where
  defaultVal = OkT

booleanT :: Tp ()
booleanT = ClassT () (ClsNm "Boolean")
integerT :: Tp ()
integerT = ClassT () (ClsNm "Integer")
floatT :: Tp ()
floatT = ClassT () (ClsNm "Float")
stringT :: Tp ()
stringT = ClassT () (ClsNm "String")

data VarDecl t = VarDecl {annotOfVarDecl :: t, nameOfVarDecl :: VarName, tpOfVarDecl :: Tp t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (VarDecl t) where
  getLoc = getLoc . annotOfVarDecl

instance HasAnnot VarDecl where
  getAnnot = annotOfVarDecl
  updateAnnot f p = p { annotOfVarDecl = f (annotOfVarDecl p)}

data Mapping t = Mapping { annotOfMapping :: t
                          , fromMapping :: VarName
                          , toMapping :: VarName}

  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)
instance HasLoc t => HasLoc (Mapping t) where
  getLoc (Mapping t _ _) = getLoc t

instance HasAnnot Mapping where
  getAnnot (Mapping a _ _) = a
  updateAnnot f (Mapping a v1 v2) = Mapping (f a) v1 v2

-- Field attributes: for example cardinality restrictions
-- data FieldAttribs = FldAtt
data FieldDecl t = FieldDecl {annotOfFieldDecl :: t
                             , nameOfFieldDecl :: FieldName
                             , tpOfFieldDecl ::  Tp t }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)
instance HasLoc t => HasLoc (FieldDecl t) where
  getLoc = getLoc . annotOfFieldDecl

instance HasAnnot FieldDecl where
  getAnnot = annotOfFieldDecl
  updateAnnot f p = p { annotOfFieldDecl = f (annotOfFieldDecl p)}

-- superclasses, list of field declarations
-- After parsing, the list of superclasses is a singleton (i.e., the declared superclass).
-- After type checking, the list of superclasses consists of the list of non-strict superclasses
-- TODO: ClassDef currently without annotation as ClassDef may be empty
--       and it is unclear how to define position information of empty elements
data ClassDef t = ClassDef { supersOfClassDef :: [ClassName]
                           , fieldsOfClassDef :: [FieldDecl t] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- declares class with ClassName and definition as of ClassDef
data ClassDecl t = ClassDecl { annotOfClassDecl :: t
                             , nameOfClassDecl :: ClassName
                             , defOfClassDecl :: ClassDef t }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)
instance HasLoc t => HasLoc (ClassDecl t) where
  getLoc = getLoc . annotOfClassDecl

instance HasAnnot ClassDecl where
  getAnnot = annotOfClassDecl
  updateAnnot f p = p { annotOfClassDecl = f (annotOfClassDecl p)}


-- Custom Classes and Preable Module
-- some custom classes - should eventually go into a prelude and not be hard-wired
-- TODO: now special treatment in Prelude.l4
-- objectC = ClassDecl (ClsNm "Object") (ClassDef Nothing [])

{-
TODO: the following should be defined in concrete syntax in a preamble.

-- QualifiedNumeric class with val field
-- TODO: should its type be IntT or a FloatT?
qualifNumC = ClassDecl (ClsNm "QualifiedNumeric")
                    (ClassDef (Just (ClsNm "Object"))
                            [FieldDecl (FldNm "val") IntT])

-- Currency as QualifiedNumeric, with specific currencies (SGD, USD) as subclasses
currencyC = ClassDecl (ClsNm "Currency")
                    (ClassDef (Just (ClsNm "QualifiedNumeric")) [])
currencyCs = [ClassDecl (ClsNm "SGD") (ClassDef (Just (ClsNm "Currency")) []),
              ClassDecl (ClsNm "USD") (ClassDef (Just (ClsNm "Currency")) [])]

-- Time as QualifiedNumeric, with Year, Month, Day etc. as subclasses
-- TODO: treatment of time needs a second thought
--       (so far no distinction between time point and duration)
timeC = ClassDecl (ClsNm "Time")
                    (ClassDef (Just (ClsNm "QualifiedNumeric")) [])
timeCs = [ClassDecl (ClsNm "Year") (ClassDef (Just (ClsNm "Time")) []),
          ClassDecl (ClsNm "Day") (ClassDef (Just (ClsNm "Time")) [])]

eventC  = ClassDecl (ClsNm "Event")
                  (ClassDef (Just (ClsNm "Object"))
                   [FieldDecl (FldNm "time") (ClassT (ClsNm "Time"))])
customCs = [objectC, qualifNumC, currencyC] ++ currencyCs ++ [timeC] ++ timeCs ++ [eventC]
-}

-- TODO: now special treatment in Prelude.l4
--customCs = [objectC]

----- Expressions
data Val
    = BoolV Bool
    | IntV Integer
    | FloatV Float
    | StringV String
    -- TODO: instead of RecordV, introduce RecordE in type Expr
    -- | RecordV ClassName [(FieldName, Val)]
    | ErrV
  deriving (Eq, Ord, Show, Read, Data, Typeable)

trueV :: Expr (Tp ())
trueV = ValE booleanT (BoolV True)
falseV :: Expr (Tp ())
falseV = ValE booleanT (BoolV False)

data Var t
      -- global variable only known by its name
    = GlobalVar { nameOfVar :: QVarName t }
    -- local variable known by its provisional name and deBruijn index.
    | LocalVar { nameOfVar :: QVarName t
               , indexOfVar :: Int }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary boolean operators
data UBoolOp = UBnot
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data UTemporalOp
  = UTAF -- always finally
  | UTAG -- always generally
  | UTEF -- exists finally
  | UTEG -- exists generally
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
    | UTemporal UTemporalOp
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- binary arithmetic operators
data BArithOp = BAadd | BAsub | BAmul | BAdiv | BAmod
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- binary comparison operators
data BComparOp = BCeq | BClt | BClte | BCgt | BCgte | BCne
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- binary boolean operators
data BBoolOp = BBimpl | BBor | BBand
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- binary operators (union of the above)
data BinOp
    = BArith BArithOp
    | BCompar BComparOp
    | BBool BBoolOp
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- operators for combining list elements
data ListOp = AndList | OrList | XorList | CommaList
    deriving (Eq, Ord, Show, Read, Data, Typeable)

data Pattern t
    = VarP (QVarName t)
    | VarListP [QVarName t]
    deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

patternLength :: Pattern t -> Int
patternLength (VarP _) = 1
patternLength (VarListP vs) = length vs

data Quantif = All | Ex
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Expr t is an expression of type t (to be determined during type checking / inference)
data Expr t
    = ValE        {annotOfExpr :: t, valOfExprValE :: Val}                       -- value
    | VarE        {annotOfExpr :: t, varOfExprVarE :: Var t}                       -- variable
    | UnaOpE      {annotOfExpr :: t, unaOpOfExprUnaOpE :: UnaOp, subEOfExprUnaOpE :: Expr t} -- unary operator
    | BinOpE      {annotOfExpr :: t, binOpOfExprBinOpE :: BinOp, subE1OfExprBinOpE :: Expr t, subE2OfExprBinOpE :: Expr t}      -- binary operator
    | IfThenElseE {annotOfExpr :: t, condOfExprIf :: Expr t, thenofExprIf :: Expr t, elseOfExprIf :: Expr t}   -- conditional
    | AppE        {annotOfExpr :: t, funOfExprAppE :: Expr t, argOfExprAppE :: Expr t}           -- function application
    | FunE        {annotOfExpr :: t, patternOfExprFunE :: Pattern t, tpOfExprFunE :: Tp t, bodyOfExprFunE :: Expr t}          -- function abstraction
    | QuantifE    {annotOfExpr :: t, quantifOfExprQ :: Quantif, varNameOfExprQ :: QVarName t, tpOfExprQ :: Tp t, bodyOfExprQ :: Expr t}  -- quantifier
    | FldAccE     {annotOfExpr :: t, subEOfExprFldAccE :: Expr t, fieldNameOfExprFldAccE :: FieldName}           -- field access
    | TupleE      {annotOfExpr :: t, componentsOfExprTupleE :: [Expr t]}                     -- tuples
    | CastE       {annotOfExpr :: t, tpOfExprCastE :: Tp t, subEOfExprCastE :: Expr t}               -- cast to type
    | ListE       {annotOfExpr :: t, listOpOfExprListE :: ListOp, componentsOfExprListE :: [Expr t]}    -- list expression
    | NotDeriv    {annotOfExpr :: t, isPosLitOfExprNotDeriv ::  Bool, subEOfExprNotDeriv :: Expr t}            -- Negation as failure "not".
                                                      -- The Bool expresses whether "not" precedes a positive literal (True)
                                                      -- or is itself classically negated (False)
                                                      -- The expresssion argument should be a predicate
                                                      -- or the application of a predicate to an atom
    deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)


childExprs :: Expr t -> [Expr t]
childExprs ex = case ex of
    ValE        _ _       -> []
    VarE        _ _       -> []
    UnaOpE      _ _ a     -> [a]
    BinOpE      _ _ a b   -> [a,b]
    IfThenElseE _ i t e   -> [i,t,e]
    AppE        _ f x     -> [f,x]
    FunE        _ _ _ x   -> [x]
    QuantifE    _ _ _ _ x -> [x]
    FldAccE     _ x _     -> [x]
    TupleE      _ xs      -> xs
    CastE       _ _ x     -> [x]
    ListE       _ _ xs    -> xs
    NotDeriv    _ _ e     -> [e]

allSubExprs :: Expr t -> [Expr t]
allSubExprs e = e : concatMap allSubExprs (childExprs e)


updAnnotOfExpr :: (a -> a) -> Expr a -> Expr a
updAnnotOfExpr f e = e {annotOfExpr = f (annotOfExpr e)}

instance HasLoc t => HasLoc (Expr t) where
  getLoc e = getLoc (annotOfExpr e)

instance HasAnnot Expr where
  getAnnot = annotOfExpr
  updateAnnot = updAnnotOfExpr

-- Cmd t is a command of type t
data Cmd t
    = Skip t                                      -- Do nothing
    | VAssign t (Var  t) (Expr t)                   -- Assignment to variable
    | FAssign t (Expr t) FieldName (Expr t)         -- Assignment to field
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)


data Rule t = Rule { annotOfRule :: t
                   , nameOfRule :: ARName
                   , instrOfRule :: KVMap
                   , varDeclsOfRule :: [VarDecl t]
                   , precondOfRule :: Expr t
                   , postcondOfRule :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (Rule t) where
  getLoc e = getLoc (annotOfRule e)

instance HasAnnot Rule where
  getAnnot = annotOfRule
  updateAnnot f p = p { annotOfRule = f (annotOfRule p)}

data Assertion t = Assertion { annotOfAssertion :: t
                             , nameOfAssertion :: ARName
                             , instrOfAssertion :: KVMap
                             , exprOfAssertion :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (Assertion t) where
  getLoc e = getLoc (annotOfAssertion e)

instance HasAnnot Assertion where
  getAnnot = annotOfAssertion
  updateAnnot f p = p { annotOfAssertion = f (annotOfAssertion p)}


----------------------------------------------------------------------
-- Definition of Timed Automata
----------------------------------------------------------------------

newtype Clock = Clock {nameOfClock :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Clock constraint of the form x < c.
-- TODO: Reconsider the Integer. Might also be a subclass of the "Time" class
data ClConstr = ClConstr Clock BComparOp Integer
  deriving (Eq, Ord, Show, Read, Data, Typeable)

newtype Loc = Loc String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Synchronization type: send or receive
data Sync = Snd | Rec
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Action in a transition: the string is the ClassName of a subclass of Event
data Action
  = Internal
  | Act ClassName Sync
  deriving (Eq, Ord, Show, Read, Data, Typeable)

actionName :: Action -> [ClassName]
actionName Internal = []
actionName (Act cn _) = [cn]

-- Transition condition: clock constraints and Boolean expression
data TransitionGuard t = TransitionGuard [ClConstr] (Expr t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- Transition action: synchronization action; clock resets; and execution of command (typically assignments)
data TransitionAction t = TransitionAction Action [Clock] (Cmd t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

transitionActionName :: TransitionAction t -> [ClassName]
transitionActionName (TransitionAction act _ _) = actionName act

-- Transition relation from location to location via Action,
-- provided [ClConstr] are satisfied; and resetting [Clock]
data Transition t = Transition {
    sourceOfTransition :: Loc
  , guardOfTransition :: TransitionGuard t
  , actionOfTransition :: TransitionAction t
  , targetOfTransition :: Loc
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- Timed Automaton having:
-- a name
-- a set of locations,
-- a set of channel types (subclasses of Event),
-- a set of clocks,
-- a transition relation,
-- a set of initial locations,
-- an invariant per location and
-- a labelling (an expression true in the location).
-- Major extension: "Labeling function" which is typically taken to be Loc -> AP -> Bool
-- for AP a type of atomic propositioons and which is here taken to be [(Loc, Expr t)].
-- Note: the set of locations, actions, clocks could in principle be inferred from the remaining info.
-- Type parameter t: type of expressions: () or Tp, see function Typing/tpExprr
data TA t =
  TA {
    annotOfTA :: t,
    nameOfTA :: String,
    locsOfTA :: [Loc],
    channelsOfTA :: [ClassName],
    clocksOfTA :: [Clock],
    transitionsOfTA :: [Transition t],
    initialLocsOfTA :: [Loc],
    invarsOfTA ::  [(Loc, [ClConstr])],
    labellingOfTA :: [(Loc, Expr t)]
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- Timed Automata System: a set of TAs running in parallel
-- Type parameter ext: Environment-specific extension
data TASys t ext = TASys [TA t] ext
  deriving (Eq, Ord, Show, Read, Data, Typeable)

{- Obsolete with record names
nameOfTA :: TA t -> String
nameOfTA (TA nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = nm

channelsOfTA :: TA t -> [ClassName]
channelsOfTA (TA nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = ta_act_clss
-}

----------------------------------------------------------------------
-- L4 Event Rules
----------------------------------------------------------------------

-- CURRENTLY NOT USED, rather see the translations in RuleToTa.hs

-- NB: Event rules as opposed to rules defining terminology etc.

data Event t
  = EventClConstr ClConstr
  | EventCond (Expr t)
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- only Must and May, assuming that Shant can be compiled away during syntax analysis
data Modality = Must | May
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- EventRule with components:
-- rule name
-- event list (interpreted conjunctively, all events of the list have to be satisfied)
-- modality
-- a non-empthy list of Parties (and not a single one). The first in the list is the one initiating the action
-- (i.e., sender), the other ones are the receivers (if any)
-- action
-- clock constraints valid in the state corresponding to the name of this rule
-- rule name corresponding to HENCE clause
-- rule name (optional) corresponding to LEST clause

data EventRule t = EvRule ARName [Event t] Modality [PartyName] Action [ClConstr] ARName ARName
  deriving (Eq, Ord, Show, Read, Data, Typeable)
