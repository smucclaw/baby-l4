{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# OPTIONS_GHC -Wpartial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Syntax where


-- Class for annotated expressions
import qualified Language.LSP.Types as J    -- TODO: is that used anywhere?
import qualified Data.List as List
import Data.Data (Data, Typeable)
import Annotation

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

newtype ClassName = ClsNm String
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype FieldName = FldNm String
  deriving (Eq, Ord, Show, Read, Data, Typeable)
newtype PartyName = PtNm String
  deriving (Eq, Ord, Show, Read, Data, Typeable)


-- TODO: has been moved to Annotation.hs
{-
class HasLoc a where
  getLoc :: a -> SRng


data Pos = Pos
  { line :: !Int
  , col  :: !Int
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)
data SRng = SRng
  { start :: Pos
  , end   :: Pos
  }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance HasLoc SRng where
  getLoc = id
-}

----- Program

data Program ct et = Program{ lexiconOfProgram :: [Mapping]
                            , classDeclsOfProgram ::  [ClassDecl ct] 
                            , globalsOfProgram :: [VarDecl] 
                            , rulesOfProgram :: [Rule et]
                            , assertionsOfProgram :: [Assertion et] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

removeAnnotations :: Program ct et -> Program ct ()
removeAnnotations = (()<$)

----- Types
data Tp
  = BoolT
  | IntT
  | ClassT ClassName
  | FunT Tp Tp
  | TupleT [Tp]
  | ErrT
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data VarDecl = VarDecl VarName Tp
  deriving (Eq, Ord, Show, Read, Data, Typeable)
data Mapping = Mapping SRng VarName VarName
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Field attributes: for example cardinality restrictions
-- data FieldAttribs = FldAtt
data FieldDecl = FieldDecl FieldName Tp -- FieldAttribs
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- superclass, list of field declarations
data ClassDef t = ClassDef { supersOfClassDef :: t
                           , fieldsOfClassDef :: [FieldDecl] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- declares class with ClassName and definition as of ClassDef
data ClassDecl t = ClassDecl { nameOfClassDecl :: ClassName
                             , defOfClassDecl :: ClassDef t }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)



-- Custom Classes and Preable Module
-- some custom classes - should eventually go into a prelude and not be hard-wired
objectC = ClassDecl (ClsNm "Object") (ClassDef Nothing [])

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

customCs = [objectC]

----- Expressions
data Val
    = BoolV Bool
    | IntV Integer
    | StringV String
    -- TODO: instead of RecordV, introduce RecordE in type Expr
    | RecordV ClassName [(FieldName, Val)]
    | ErrV
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Var
    = GlobalVar VarName             -- global variable only known by its name
    | LocalVar VarName Int          -- local variable known by its provisional name and deBruijn index.
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- unary boolean operators
data UBoolOp = UBneg
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

data Quantif = All | Ex
    deriving (Eq, Ord, Show, Read, Data, Typeable)


-- Expr t is an expression of type t (to be determined during type checking / inference)
data Expr t
    = ValE        t Val                          -- value
    | VarE        t Var                          -- variable
    | UnaOpE      t UnaOp (Expr t)               -- unary operator
    | BinOpE      t BinOp (Expr t) (Expr t)      -- binary operator
    | IfThenElseE t (Expr t) (Expr t) (Expr t)   -- conditional
    | AppE        t (Expr t) (Expr t)            -- function application
    | FunE        t Pattern Tp (Expr t)          -- function abstraction
    | QuantifE    t Quantif VarName Tp (Expr t)  -- quantifier
    -- | ClosE       SRng t [(VarName, Expr t)] (Expr t) -- closure  (not externally visible)
    | FldAccE     t (Expr t) FieldName           -- field access
    | TupleE      t [Expr t]                     -- tuples
    | CastE       t Tp (Expr t)                  -- cast to type
    | ListE       t ListOp [Expr t]              -- list expression
    | NotDeriv    t Bool Var (Expr t)            -- Negation as failure "not". 
                                                      -- The Bool expresses whether "not" precedes a positive literal (True)
                                                      -- or is itself classically negated (False)
    deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

    {-
instance HasLoc (Expr t) where
  getLoc x = case x of
    ValE        loc _ _       -> loc
    VarE        loc _ _       -> loc
    UnaOpE      loc _ _ _     -> loc
    BinOpE      loc _ _ _ _   -> loc
    IfThenElseE loc _ _ _ _   -> loc
    AppE        loc _ _ _     -> loc
    FunE        loc _ _ _ _   -> loc
    QuantifE    loc _ _ _ _ _ -> loc
    FldAccE     loc _ _ _     -> loc
    TupleE      loc _ _       -> loc
    CastE       loc _ _ _     -> loc
    ListE       loc _ _ _     -> loc
    NotDeriv    loc _ _ _ _   -> loc 
-}

childExprs :: Expr t -> [Expr t]
childExprs x = case x of
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
    NotDeriv    _ _ _ e   -> [e]

annotOfExpr :: Expr t -> t
annotOfExpr x = case x of
  ValE        t _       -> t
  VarE        t _       -> t
  UnaOpE      t _ _     -> t
  BinOpE      t _ _ _   -> t
  IfThenElseE t _ _ _   -> t
  AppE        t _ _     -> t
  FunE        t _ _ _   -> t
  QuantifE    t _ _ _ _ -> t
  FldAccE     t _ _     -> t
  TupleE      t _       -> t
  CastE       t _ _     -> t
  ListE       t _ _     -> t
  NotDeriv    t _ _ _   -> t

instance HasLoc t => HasLoc (Expr t) where
  getLoc e = getLoc (annotOfExpr e)

-- Cmd t is a command of type t
data Cmd t
    = Skip                                      -- Do nothing
    | VAssign Var (Expr t)                   -- Assignment to variable
    | FAssign (Expr t) FieldName (Expr t)         -- Assignment to field
  deriving (Eq, Ord, Show, Read, Data, Typeable)


data Rule t = Rule RuleName [VarDecl] (Expr t) (Expr t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

data Assertion t = Assertion (Expr t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

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
