{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wpartial-fields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module L4.Syntax where


-- Class for annotated expressions
import Data.Data (Data, Typeable)
import L4.Annotation
import L4.KeyValueMap
import Data.Maybe ( mapMaybe, isJust )
import Data.Hashable
import GHC.Generics (Generic)

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
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable ClassName

newtype FieldName = FldNm {stringOfFieldName :: String}
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable FieldName

newtype PartyName = PtNm {stringOfPartyName :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- a qualified var name has an annotation (contrary to a simple var name)
data QVarName t = QVarName {annotOfQVarName :: t, nameOfQVarName :: VarName}
  deriving (Eq, Generic, Ord, Show, Read, Functor, Data, Typeable)

instance Hashable t => Hashable (QVarName t)

instance HasLoc t => HasLoc (QVarName t) where
  getLoc v = getLoc (annotOfQVarName v)

data Description = Descr {predOfDescription :: String , argsOfDescription :: [String]}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

--parseDescription :: String -> Description
--parseDescription "{Player} participates in {Game}" = Descr "participates in" ["Player", "Game"]
--parseDescription _ = Descr "I am super high" []
--parseDescription "{Game} participates in" = Descr "participates in" ["Game"]

-- todo:
-- get all string between {} and stick in an array for argsOfDescription
-- concat everything else in predOfDescription as a string

parseDescription :: String -> Description
parseDescription x
  | '{' `elem` x =
          let allWords = words $ filter (`notElem` "{}") x
          in Descr {predOfDescription = unwords (tail (init allWords)), argsOfDescription =[head allWords, last allWords]}
  | otherwise = Descr {predOfDescription = x, argsOfDescription = []}

{-
Basic description:

participate_in : Player -> Game -> Bool
'participates in', implicitly this means 'Player participates in Game'

participate_in : Game -> Player -> Bool
'participates in', implicitly this means 'Game participates in Player'

Explicit: '{Player} participates in {Game}'
-}


----- Program

data TopLevelElement t
  = MappingTLE (Mapping t)
  | ClassDeclTLE (ClassDecl t)
  | VarDeclTLE (VarDecl t)
  | VarDefnTLE (VarDefn t)
  | RuleTLE (Rule t)
  | AssertionTLE (Assertion t)
  | AutomatonTLE (TA t)
  | SystemTLE (TASys t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

getAnnotOfTLE :: TopLevelElement t -> t
getAnnotOfTLE e = case e of
     MappingTLE mp -> annotOfMapping mp
     ClassDeclTLE cd -> annotOfClassDecl cd
     VarDeclTLE vd -> annotOfVarDecl vd
     VarDefnTLE vd -> annotOfVarDefn vd
     RuleTLE ru -> annotOfRule ru
     AssertionTLE as -> annotOfAssertion as
     AutomatonTLE ta -> annotOfTA ta
     SystemTLE s -> annotOfSys s

updateAnnotOfTLE :: (t -> t) -> TopLevelElement t -> TopLevelElement t
updateAnnotOfTLE f e = case e of
     MappingTLE mp -> MappingTLE $ mp { annotOfMapping = f (annotOfMapping mp) }
     ClassDeclTLE cd -> ClassDeclTLE $ cd { annotOfClassDecl = f (annotOfClassDecl cd) }
     VarDeclTLE vd -> VarDeclTLE $ vd { annotOfVarDecl = f (annotOfVarDecl vd) }
     VarDefnTLE vd -> VarDefnTLE $ vd { annotOfVarDefn = f (annotOfVarDefn vd) }
     RuleTLE ru -> RuleTLE $ ru { annotOfRule = f (annotOfRule ru) }
     AssertionTLE as -> AssertionTLE $ as { annotOfAssertion = f (annotOfAssertion as) }
     AutomatonTLE ta -> AutomatonTLE $ ta { annotOfTA = f (annotOfTA ta) }
     SystemTLE s -> SystemTLE $ s { annotOfSys = f (annotOfSys s) }

instance HasLoc t => HasLoc (TopLevelElement t) where
  getLoc = getLoc . getAnnotOfTLE

instance HasAnnot TopLevelElement where
  getAnnot = getAnnotOfTLE
  updateAnnot = updateAnnotOfTLE

data Program t = Program { annotOfProgram :: t
                               , elementsOfProgram :: [TopLevelElement t] }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasAnnot Program where
  getAnnot = annotOfProgram
  updateAnnot f p = p { annotOfProgram = f (annotOfProgram p)}


getMapping :: TopLevelElement t -> Maybe (Mapping t)
getMapping (MappingTLE e) = Just e
getMapping _ = Nothing

getClassDecl :: TopLevelElement t -> Maybe (ClassDecl t)
getClassDecl (ClassDeclTLE e) = Just e
getClassDecl _ = Nothing

getVarDecl :: TopLevelElement t -> Maybe (VarDecl t)
getVarDecl (VarDeclTLE e) = Just e
getVarDecl _ = Nothing

getVarDefn :: TopLevelElement t -> Maybe (VarDefn t)
getVarDefn (VarDefnTLE e) = Just e
getVarDefn _ = Nothing

getRule :: TopLevelElement t -> Maybe (Rule t)
getRule (RuleTLE e) = Just e
getRule _ = Nothing

getAssertion :: TopLevelElement t -> Maybe (Assertion t)
getAssertion (AssertionTLE e) = Just e
getAssertion _ = Nothing

getAutomaton :: TopLevelElement t -> Maybe (TA t)
getAutomaton (AutomatonTLE e) = Just e
getAutomaton _ = Nothing

getSystem :: TopLevelElement t -> Maybe (TASys t)
getSystem (SystemTLE e) = Just e
getSystem _ = Nothing

typeOfTLE :: (t -> Maybe a) -> t -> Bool
typeOfTLE g = isJust . g

lexiconOfProgram :: Program t -> [Mapping t]
lexiconOfProgram = mapMaybe getMapping . elementsOfProgram

classDeclsOfProgram :: Program t -> [ClassDecl t]
classDeclsOfProgram = mapMaybe getClassDecl . elementsOfProgram

varDeclsOfProgram :: Program t -> [VarDecl t]
varDeclsOfProgram = mapMaybe getVarDecl . elementsOfProgram

varDefnsOfProgram :: Program t -> [VarDefn t]
varDefnsOfProgram = mapMaybe getVarDefn . elementsOfProgram

rulesOfProgram :: Program t -> [Rule t]
rulesOfProgram = mapMaybe getRule . elementsOfProgram

assertionsOfProgram :: Program t -> [Assertion t]
assertionsOfProgram = mapMaybe getAssertion . elementsOfProgram

automataOfProgram :: Program t -> [TA t]
automataOfProgram = mapMaybe getAutomaton . elementsOfProgram

systemOfProgram :: Program t -> [TASys t]
systemOfProgram = mapMaybe getSystem . elementsOfProgram

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


----- Types
-- TODO: also types have to be annotated with position information
-- for the parser to do the right job
data Tp t
  = ClassT {annotOfTp :: t, classNameOfTp :: ClassName}  -- class Integer, Boolean ...
  | FunT {annotOfTp :: t, paramTp :: Tp t, resultTp :: Tp t}   -- T1 -> T2
  | TupleT {annotOfTp :: t, componentsOfTpTupleT :: [Tp t]}    -- (T1, T2)
  | ErrT
  | OkT        -- fake type appearing in constructs (classes, rules etc.) that do not have a genuine type
  | KindT
  deriving (Eq, Generic, Ord, Show, Read, Functor, Data, Typeable)

instance Hashable t => Hashable (Tp t)

instance HasLoc t => HasLoc (Tp t) where
  getLoc e = getLoc (annotOfTp e)

instance HasDefault (Tp t) where
  defaultVal = OkT


pattern BooleanC :: ClassName
pattern BooleanC = ClsNm "Boolean"
pattern ClassC :: ClassName
pattern ClassC = ClsNm "Class"
pattern FloatC :: ClassName
pattern FloatC = ClsNm "Float"
pattern IntegerC :: ClassName
pattern IntegerC = ClsNm "Integer"
pattern NumberC :: ClassName
pattern NumberC = ClsNm "Number"
pattern StateC :: ClassName
pattern StateC = ClsNm "State"
pattern StringC :: ClassName
pattern StringC = ClsNm "String"
pattern TimeC :: ClassName
pattern TimeC = ClsNm "Time"

-- TODO: Replace these with pattern synonyms like those in NormalizeSyntax.hs
pattern BooleanT :: Tp ()
pattern BooleanT = ClassT () BooleanC
pattern FloatT :: Tp ()
pattern FloatT = ClassT () FloatC
pattern IntegerT :: Tp ()
pattern IntegerT = ClassT () IntegerC
pattern StateT :: Tp ()
pattern StateT = ClassT () StateC
pattern StringT :: Tp ()
pattern StringT = ClassT () StringC
pattern TimeT :: Tp ()
pattern TimeT = ClassT () TimeC
pattern NumberT :: Tp ()
pattern NumberT = ClassT () NumberC


data VarDecl t = VarDecl {annotOfVarDecl :: t, nameOfVarDecl :: VarName, tpOfVarDecl :: Tp t}
  deriving (Eq, Generic, Ord, Show, Read, Functor, Data, Typeable)

instance Hashable t => Hashable (VarDecl t)

data VarDefn t = VarDefn {annotOfVarDefn :: t, nameOfVarDefn :: VarName, tpOfVarDefn :: Tp t, bodyOfVarDefn :: Expr t}
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasLoc t => HasLoc (VarDecl t) where
  getLoc = getLoc . annotOfVarDecl

instance HasLoc t => HasLoc (VarDefn t) where
  getLoc = getLoc . annotOfVarDefn

instance HasAnnot VarDefn where
  getAnnot = annotOfVarDefn
  updateAnnot f p = p { annotOfVarDefn = f (annotOfVarDefn p)}

instance HasAnnot VarDecl where
  getAnnot = annotOfVarDecl
  updateAnnot f p = p { annotOfVarDecl = f (annotOfVarDecl p)}
data Mapping t = Mapping { annotOfMapping :: t
                          , fromMapping :: VarName
                          , toMapping :: Description}

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


----- Expressions
data Val
    = BoolV Bool
    | IntV Integer
    | FloatV Float
    | StringV String
    -- TODO: instead of RecordV, introduce RecordE in type Expr
    -- | RecordV ClassName [(FieldName, Val)]
    | ErrV
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable Val

trueV :: Expr (Tp ())
trueV = ValE BooleanT (BoolV True)
falseV :: Expr (Tp ())
falseV = ValE BooleanT (BoolV False)

trueVNoType :: Expr ()
trueVNoType = ValE () (BoolV True)
falseVNoType :: Expr ()
falseVNoType = ValE () (BoolV False)

-- TODO: in most cases, the annotation of QVarName seems redundant. 
data Var t
      -- global variable only known by its name
    = GlobalVar { nameOfVar :: QVarName t }
    -- local variable known by its provisional name and deBruijn index.
    | LocalVar { nameOfVar :: QVarName t
               , indexOfVar :: Int }
  deriving (Eq, Generic, Ord, Show, Read, Functor, Data, Typeable)

instance Hashable t => Hashable (Var t)

-- unary arithmetic operators
data UArithOp = UAminus
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable UArithOp

-- unary boolean operators
data UBoolOp = UBnot
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable UBoolOp

data UTemporalOp
  = UTAF -- always finally
  | UTAG -- always generally
  | UTEF -- exists finally
  | UTEG -- exists generally
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable UTemporalOp

-- unary operators (union of the above)
data UnaOp
    = UArith UArithOp
    | UBool UBoolOp
    | UTemporal UTemporalOp
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable UnaOp

-- binary arithmetic operators
data BArithOp = BAadd | BAsub | BAmul | BAdiv | BAmod
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable BArithOp

-- binary comparison operators
data BComparOp = BCeq | BClt | BClte | BCgt | BCgte | BCne
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable BComparOp

-- binary boolean operators
data BBoolOp = BBimpl | BBor | BBand
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable BBoolOp

-- binary operators (union of the above)
data BinOp
    = BArith BArithOp
    | BCompar BComparOp
    | BBool BBoolOp
  deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable BinOp

-- operators for combining list elements
data ListOp = AndList | OrList | XorList | CommaList
    deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable ListOp

data Pattern t
    = VarP (QVarName t)
    | VarListP [QVarName t]
    deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

patternLength :: Pattern t -> Int
patternLength (VarP _) = 1
patternLength (VarListP vs) = length vs

data Quantif = All | Ex
    deriving (Eq, Generic, Ord, Show, Read, Data, Typeable)

instance Hashable Quantif

-- Expr t is an expression of type t (to be determined during type checking / inference)
data Expr t
    = ValE        {annotOfExpr :: t, valOfExprValE :: Val}                       -- value
    | VarE        {annotOfExpr :: t, varOfExprVarE :: Var t}                       -- variable
    | UnaOpE      {annotOfExpr :: t, unaOpOfExprUnaOpE :: UnaOp, subEOfExprUnaOpE :: Expr t} -- unary operator
    | BinOpE      {annotOfExpr :: t, binOpOfExprBinOpE :: BinOp, subE1OfExprBinOpE :: Expr t, subE2OfExprBinOpE :: Expr t}      -- binary operator
    | IfThenElseE {annotOfExpr :: t, condOfExprIf :: Expr t, thenofExprIf :: Expr t, elseOfExprIf :: Expr t}   -- conditional
    | AppE        {annotOfExpr :: t, funOfExprAppE :: Expr t, argOfExprAppE :: Expr t}           -- function application
    | FunE        {annotOfExpr :: t, varOfFunE :: VarDecl t, bodyOfFunE :: Expr t}          -- function abstraction
    | QuantifE    {annotOfExpr :: t, quantifOfExprQ :: Quantif, varOfExprQ :: VarDecl t, bodyOfExprQ :: Expr t}  -- quantifier
    | FldAccE     {annotOfExpr :: t, subEOfExprFldAccE :: Expr t, fieldNameOfExprFldAccE :: FieldName}           -- field access
    | TupleE      {annotOfExpr :: t, componentsOfExprTupleE :: [Expr t]}                     -- tuples
    | CastE       {annotOfExpr :: t, tpOfExprCastE :: Tp t, subEOfExprCastE :: Expr t}               -- cast to type
    | ListE       {annotOfExpr :: t, listOpOfExprListE :: ListOp, componentsOfExprListE :: [Expr t]}    -- list expression
    deriving (Eq, Generic, Ord, Show, Read, Functor, Data, Typeable)

instance Hashable t => Hashable (Expr t)


childExprs :: Expr t -> [Expr t]
childExprs ex = case ex of
    ValE        _ _       -> []
    VarE        _ _       -> []
    UnaOpE      _ _ a     -> [a]
    BinOpE      _ _ a b   -> [a,b]
    IfThenElseE _ i t e   -> [i,t,e]
    AppE        _ f x     -> [f,x]
    FunE        _ _ x   -> [x]
    QuantifE    _ _ _ x -> [x]
    FldAccE     _ x _     -> [x]
    TupleE      _ xs      -> xs
    CastE       _ _ x     -> [x]
    ListE       _ _ xs    -> xs

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

type ChannelName = String
newtype Clock = Clock {nameOfClock :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Clock constraint of the form x < c.
-- TODO: Reconsider the Integer. Might also be a subclass of the "Time" class
data ClConstr = ClConstr Clock BComparOp Integer
  deriving (Eq, Ord, Show, Read, Data, Typeable)

newtype Loc = Loc {nameOfLoc :: String}
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SyncMode = Broadcast | Send | Receive
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Sync = Sync {eventNameOfSync :: ChannelName, modeOfSync :: SyncMode }
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- Transition condition: clock constraints and Boolean expression
data TransitionGuard t = TransitionGuard [ClConstr] (Maybe (Expr t))
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- Transition action: synchronization action; clock resets; and execution of command (typically assignments)
data TransitionAction t = TransitionAction [Clock] (Cmd t)
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

{- TODO: obsolete
transitionActionName :: TransitionAction t -> [ClassName]
transitionActionName (TransitionAction act _ _) = actionName act
-}

-- Transition relation from location to location via Action,
-- provided [ClConstr] are satisfied; and resetting [Clock]
data Transition t = Transition {
    sourceOfTransition :: Loc
  , guardOfTransition :: TransitionGuard t
  , syncOfTransition :: Maybe Sync
  , actionOfTransition :: TransitionAction t
  , targetOfTransition :: Loc
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- Timed Automaton having:
-- a name
-- a set of locations,
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
    clocksOfTA :: [Clock],
    transitionsOfTA :: [Transition t],
    urgentLocsOfTA :: [Loc],
    initialLocOfTA :: Loc,
    invarsOfTA ::  [(Loc, [ClConstr])],
    labellingOfTA :: [(Loc, Expr t)]
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasAnnot TA where
  getAnnot = annotOfTA
  updateAnnot f p = p { annotOfTA = f (annotOfTA p)}

-- Timed Automata System: a set of TAs running in parallel
-- Type parameter ext: Environment-specific extension
data TASys t = TASys {
  annotOfSys :: t,
  nameOfTASys :: String,
  declsOfSys :: [VarDecl t],
  channelsOfSys :: [ChannelName],
  automataOfSys :: [TA t]
  }
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

instance HasAnnot TASys where
  getAnnot = annotOfSys
  updateAnnot f p = p { annotOfSys = f (annotOfSys p)}

----------------------------------------------------------------------
-- L4 Event Rules
----------------------------------------------------------------------
{-
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
-}
