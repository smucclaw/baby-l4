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


----------------------------------------------------------------------
-- Definition of expressions
----------------------------------------------------------------------


----- Names
type VarName = String
type RuleName = String
-- newtype VarName = VarName String
--   deriving (Eq, Ord, Show, Read, Data, Typeable)
-- newtype RuleName = RuleName String
--   deriving (Eq, Ord, Show, Read, Data, Typeable)

newtype ClassName = ClsNm {stringOfClassName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype FieldName = FldNm {stringOfFieldName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype PartyName = PtNm {stringOfPartyName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)


----- Program

data Program t = Program{ annotOfProgram :: t
                            , lexiconOfProgram :: [Mapping t]
                            , classDeclsOfProgram ::  [ClassDecl t]
                            , globalsOfProgram :: [VarDecl t]
                            , rulesOfProgram :: [Rule t]
                            , assertionsOfProgram :: [Assertion t] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

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
  deriving (Eq, Ord, Show, Read, Data, Typeable)


----- Types
-- TODO: also types have to be annotated with position information
-- for the parser to do the right job
data Tp t
  = ClassT {annotOfTp :: t, classNameOfTp :: ClassName}
  | FunT {annotOfTp :: t, funTp :: Tp t, argTp :: Tp t}
  | TupleT {annotOfTp :: t, componentsOfTpTupleT :: [Tp t]}
  | ErrT {causeOfTpErrT :: ErrorCause}
  | OkT        -- fake type appearing in constructs (classes, rules etc.) that do not have a genuine type
  | KindT
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (Tp t) where
  getLoc e = getLoc (annotOfTp e)

--------------------------------------
-- Experimental
data LocTypeAnnotInst = LocTypeAnnotInst { locAnnotI :: SRng
                                        , typeAnnotI :: Tp SRng
}
  deriving (Eq, Ord, Show, Read, Data, Typeable)


-- types occurring in source code (having location annotations in their types)
data family SourceTp a
data instance SourceTp SRng = STSRng (Tp SRng)
data instance SourceTp (LocTypeAnnot (Tp())) = STLTA (LocTypeAnnot (Tp SRng))
data instance SourceTp (Tp()) = STTp (Tp SRng)

-- types inferred during typechecking 
data family InferTp a
data instance InferTp SRng = ITSRng SRng
data instance InferTp (LocTypeAnnot (Tp())) = ITLTA (LocTypeAnnot (Tp()))
data instance InferTp (Tp()) = ITTp (Tp())


-- types inferred during typechecking 
data family NoTp a
data instance NoTp SRng = NTSRng SRng
data instance NoTp (LocTypeAnnot (Tp())) = NTLTA SRng
data instance NoTp (Tp()) = NTTp ()


-- tpExpr :: Environment t -> Expr SRng -> Expr (LocTypeAnnot (Tp SRng))
-- QuantifE    {annotOfExpr :: (InferTp t), quantifOfExprQ :: Quantif, varNameOfExprQ :: VarName, tpOfExprQ :: (SourceTp t), bodyOfExprQ :: Expr t}
--------------------------------------

booleanT :: Tp ()
booleanT = ClassT () (ClsNm "Boolean")
integerT :: Tp ()
integerT = ClassT () (ClsNm "Integer")
stringT :: Tp ()
stringT = ClassT () (ClsNm "String")

data VarDecl t = VarDecl {annotOfVarDecl :: t, nameOfVarDecl :: VarName, tpOfVarDecl :: Tp t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (VarDecl t) where
  getLoc = getLoc . annotOfVarDecl

instance HasAnnot VarDecl where
  getAnnot = annotOfVarDecl
  updateAnnot f p = p { annotOfVarDecl = f (annotOfVarDecl p)}

data Mapping t = Mapping t VarName VarName
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
    | StringV String
    -- TODO: instead of RecordV, introduce RecordE in type Expr
    | RecordV ClassName [(FieldName, Val)]
    | ErrV
  deriving (Eq, Ord, Show, Read, Data, Typeable)

trueV :: Expr (Tp ())
trueV = ValE booleanT (BoolV True)
falseV :: Expr (Tp ())
falseV = ValE booleanT (BoolV False)

data Var
      -- global variable only known by its name
    = GlobalVar { nameOfVar :: VarName }
    -- local variable known by its provisional name and deBruijn index.
    | LocalVar { nameOfVar :: VarName
               , indexOfVar :: Int }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary boolean operators
data UBoolOp = UBnot
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
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

data Pattern
    = VarP String
    | VarListP [String]
    deriving (Eq, Ord, Show, Read, Data, Typeable)

patternLength :: Pattern -> Int
patternLength (VarP _) = 1
patternLength (VarListP vs) = length vs

data Quantif = All | Ex
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Expr t is an expression of type t (to be determined during type checking / inference)
data Expr t
    = ValE        {annotOfExpr :: t, valOfExprValE :: Val}                       -- value
    | VarE        {annotOfExpr :: t, varOfExprVarE :: Var}                       -- variable
    | UnaOpE      {annotOfExpr :: t, unaOpOfExprUnaOpE :: UnaOp, subEOfExprUnaOpE :: Expr t} -- unary operator
    | BinOpE      {annotOfExpr :: t, binOpOfExprBinOpE :: BinOp, subE1OfExprBinOpE :: Expr t, subE2OfExprBinOpE :: Expr t}      -- binary operator
    | IfThenElseE {annotOfExpr :: t, condOfExprIf :: Expr t, thenofExprIf :: Expr t, elseOfExprIf :: Expr t}   -- conditional
    | AppE        {annotOfExpr :: t, funOfExprAppE :: Expr t, argOfExprAppE :: Expr t}           -- function application
    | FunE        {annotOfExpr :: t, patternOfExprFunE :: Pattern, tpOfExprFunE :: Tp t, bodyOfExprFunE :: Expr t}          -- function abstraction
    | QuantifE    {annotOfExpr :: t, quantifOfExprQ :: Quantif, varNameOfExprQ :: VarName, tpOfExprQ :: Tp t, bodyOfExprQ :: Expr t}  -- quantifier
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
    | VAssign t Var (Expr t)                   -- Assignment to variable
    | FAssign t (Expr t) FieldName (Expr t)         -- Assignment to field
  deriving (Eq, Ord, Show, Read, Data, Typeable)


data Rule t = Rule { annotOfRule :: t
                   , nameOfRule :: RuleName
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

data Clock = Cl String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Clock constraint of the form x < c.
-- TODO: Reconsider the Integer. Might also be a subclass of the "Time" class
data ClConstr = ClCn Clock BComparOp Integer
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Loc = Lc String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Synchronization type: send or receive
data Sync = Snd | Rec
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Action in a transition: the string is the ClassName of a subclass of Event
data Action
  = Internal
  | Act ClassName Sync
  deriving (Eq, Ord, Show, Read, Data, Typeable)

action_name :: Action -> [ClassName]
action_name Internal = []
action_name (Act cn s) = [cn]

-- Transition condition: clock constraints and Boolean expression
data TransitionCond t = TransCond [ClConstr] (Expr t)
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Transition action: synchronization action; clock resets; and execution of command (typically assignments)
data TransitionAction t = TransAction Action [Clock] (Cmd t)
  deriving (Eq, Ord, Show, Read, Data, Typeable)

transition_action_name :: TransitionAction t -> [ClassName]
transition_action_name (TransAction act _ _) = action_name act

-- Transition relation from location to location via Action,
-- provided [ClConstr] are satisfied; and resetting [Clock]
data Transition t = Trans Loc (TransitionCond t) (TransitionAction t) Loc
  deriving (Eq, Ord, Show, Read, Data, Typeable)

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
data TA t = TmdAut String [Loc] [ClassName] [Clock] [Transition t] [Loc] [(Loc, [ClConstr])] [(Loc, Expr t)]
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Timed Automata System: a set of TAs running in parallel
-- Type parameter ext: Environment-specific extension
data TASys t ext = TmdAutSys [TA t] ext
  deriving (Eq, Ord, Show, Read, Data, Typeable)

name_of_ta :: TA t -> String
name_of_ta (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = nm

channels_of_ta :: TA t -> [ClassName]
channels_of_ta (TmdAut nm ta_locs ta_act_clss ta_clks trans init_locs invs lbls) = ta_act_clss


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

data EventRule t = EvRule RuleName [Event t] Modality [PartyName] Action [ClConstr] RuleName (Maybe RuleName)
  deriving (Eq, Ord, Show, Read, Data, Typeable)
