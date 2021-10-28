{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# OPTIONS_GHC -Wpartial-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- Common syntax manipulations / expression transformations
module SyntaxManipulation where

--import Data.Data (Data, Typeable)

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
--import Annotation
--import KeyValueMap
import L4.Syntax
--import L4.Typing (eraseAnn, getTypeOfExpr)
--import PrintProg (printExpr, renameAndPrintExpr)
import Syntax (UnaOp(UTemporal))


----------------------------------------------------------------------
-- Operations on types, checking term properties
----------------------------------------------------------------------

liftType :: Tp () -> Tp (Tp ())
liftType t = KindT <$ t

forceResultTp :: Tp () -> Tp ()
forceResultTp (FunT _ _ptp rtp) = rtp
forceResultTp _ = ErrT

isLocalVar :: Var t -> Bool
isLocalVar (LocalVar _ _) = True
isLocalVar _ = False

-- mkFunTp [T1 .. Tn] T: construct function type T1 -> ... Tn -> T
mkFunTp :: [Tp ()] -> Tp () -> Tp ()
mkFunTp ts t = foldr (FunT ()) t ts

----------------------------------------------------------------------
-- Logical infrastructure: macros for simplifying formula construction
----------------------------------------------------------------------
mkIntConst :: Integer -> Expr (Tp())
mkIntConst f = ValE IntegerT (IntV f)

mkFloatConst :: Float -> Expr (Tp())
mkFloatConst f = ValE FloatT (FloatV f)

mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}

-- decomposes a function type T1 -> T2 ... -> Tn -> Tres
-- into ([T1, ... Tn], Tres)
spine :: [Tp t] -> Tp t -> ([Tp t], Tp t)
spine acc (FunT _ t1 t2) = spine (t1:acc) t2
spine acc t = (reverse acc, t)

-- Decompose application (f a1 .. an) into (f, [a1 .. an])
-- Typically called with [] as first argument
appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])
appToFunArgs acc (AppE _ f a) = appToFunArgs (a:acc) f
appToFunArgs acc t = (t, acc)

-- compose (f, [a1 .. an]) to (f a1 .. an)
funArgsToApp :: Expr (Tp ()) -> [Expr (Tp ())] -> Expr (Tp ())
funArgsToApp = foldl (\ f -> AppE (forceResultTp (annotOfExpr f)) f)

applyVars :: Var (Tp()) -> [Var (Tp())] -> Expr (Tp())
applyVars f args = funArgsToApp (mkVarE f) (map mkVarE args)

notExpr :: Expr (Tp ()) -> Expr (Tp())
notExpr = UnaOpE BooleanT (UBool UBnot)

conjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
conjExpr = BinOpE BooleanT (BBool BBand)

disjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
disjExpr = BinOpE BooleanT (BBool BBor)

implExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
implExpr = BinOpE BooleanT (BBool BBimpl)

conjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
conjsExpr [] = trueV
conjsExpr [e] = e
conjsExpr (e:es) = conjExpr e (conjsExpr es)

disjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
disjsExpr [] = falseV
disjsExpr [e] = e
disjsExpr (e:es) = disjExpr e (disjsExpr es)

eqExpr  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
eqExpr = BinOpE BooleanT (BCompar BCeq)

gteExpr  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
gteExpr = BinOpE BooleanT (BCompar BCgte)

mkEq :: Var (Tp()) -> Var (Tp()) -> Expr (Tp())
mkEq v1 v2 = eqExpr (mkVarE v1) (mkVarE v2)

-- Decompose list of successive applications of the same binary operator
-- for example, decomposeBinop (&&) (A && (B||C) & (D || (E&&F))) = [A, B||C, D || (E&&F)]
decomposeBinop :: BinOp -> Expr t -> [Expr t]
decomposeBinop bop e@(BinOpE _ bop' e1 e2) =
    if bop == bop'
    then decomposeBinop bop e1 ++ decomposeBinop bop e2
    else [e]
decomposeBinop _ e = [e]

-- lps and lpconcats are two different forms of list products used for computing cnf / dnf

lp :: Applicative f => f a -> f [a] -> f [a]
lp xs yss = fmap (:) xs <*> yss

-- Multiply out a list of lists by forming all possible combinations of the elements of the lists.
-- Example: lps [[0, 1], [10, 11, 12], [20, 21]]
-- [[0,10,20],[0,10,21],[0,11,20],[0,11,21],[0,12,20],[0,12,21],[1,10,20],[1,10,21],[1,11,20],[1,11,21],[1,12,20],[1,12,21]]
lps :: Foldable t => t [a] -> [[a]]
lps = foldr lp [[]]

lpconcat :: Applicative f => f [a] -> f [a] -> f [a]
lpconcat xs yss = fmap (++) xs <*> yss

-- Multiply out a list of lists of lists by concatenating combinations of sublists
-- Example: lpconcats [[[0, 1], [10, 11, 12]], [[20, 21]]]
-- [[0,1,20,21],[10,11,12,20,21]]
lpconcats :: Foldable t => t [[a]] -> [[a]]
lpconcats = foldr lpconcat [[]]


-- conjunctive normal form: conjunction of disjunctions
-- The commented line alone (together with the non-recursive terminal case) would be a correct implementation
-- but produces a complex and redundant cnf
cnf :: Expr (Tp()) -> [[Expr (Tp())]]
cnf e@BinOpE {binOpOfExprBinOpE = (BBool BBimpl)} = cnf (nnf True e)
--cnf e@BinOpE {binOpOfExprBinOpE = (BBool _)} = concatMap (lps . dnf) (decomposeBinop (BBool BBand) e)
cnf e@BinOpE {binOpOfExprBinOpE = (BBool BBand)} = concatMap cnf (decomposeBinop (BBool BBand) e)
cnf e@BinOpE {binOpOfExprBinOpE = (BBool BBor)} = lpconcats (map cnf (decomposeBinop (BBool BBor) e))
cnf e = [[e]]

-- disjunctive normal form: disjunction of conjunctions
dnf :: Expr (Tp()) -> [[Expr (Tp())]]
dnf e@BinOpE {binOpOfExprBinOpE = (BBool BBimpl)} = dnf (nnf True e)
--dnf e@BinOpE {binOpOfExprBinOpE = (BBool _)} =  concatMap (lps . cnf) (decomposeBinop (BBool BBor) e)
dnf e@BinOpE {binOpOfExprBinOpE = (BBool BBor)} =  concatMap dnf (decomposeBinop (BBool BBor) e)
dnf e@BinOpE {binOpOfExprBinOpE = (BBool BBand)} =  lpconcats (map dnf (decomposeBinop (BBool BBand) e))
dnf e = [[e]]


negateTemporal :: Bool -> UTemporalOp -> UTemporalOp
negateTemporal sign UTAF = if sign then UTAF else UTEG
negateTemporal sign UTEG = if sign then UTEG else UTAF

negateTemporal sign UTAG = if sign then UTAG else UTEF
negateTemporal sign UTEF = if sign then UTEF else UTAG

negateBinOp :: Bool -> BinOp -> BinOp
negateBinOp sign (BCompar BCeq) = if sign then BCompar BCeq else BCompar BCne
negateBinOp sign (BCompar BCne) = if sign then BCompar BCne else BCompar BCeq

negateBinOp sign (BCompar BClt) = if sign then BCompar BClt else BCompar BCgte
negateBinOp sign (BCompar BCgte) = if sign then BCompar BCgte else BCompar BClt

negateBinOp sign (BCompar BClte) = if sign then BCompar BClte else BCompar BCgt
negateBinOp sign (BCompar BCgt) = if sign then BCompar BCgt else BCompar BClte

negateBinOp sign (BBool BBor) = if sign then BBool BBor else BBool BBand
negateBinOp sign (BBool BBand) = if sign then BBool BBand else BBool BBor

negateBinOp _ b = b   -- this would in fact be a type error

negateQuantif :: Bool -> Quantif -> Quantif
negateQuantif sign All = if sign then All else Ex
negateQuantif sign Ex  = if sign then Ex else All

-- compute negation normal form of a boolean expression.
-- nnf True e computes the nnf of e;  nnf False e computes the nnf of (not e)
-- Implication a --> b is dissolved into not a || b
nnf :: Bool -> Expr (Tp()) -> Expr (Tp())
nnf sign e = case e of
  ValE t (BoolV bv) -> if sign then e else ValE t (BoolV (not bv))
  UnaOpE _ (UBool UBnot) ex -> nnf (not sign) ex
  UnaOpE t (UTemporal ut) ex -> UnaOpE t (UTemporal (negateTemporal sign ut)) (nnf sign ex)
  BinOpE t (BBool BBimpl) ex ex' -> BinOpE t (negateBinOp sign (BBool BBor)) (nnf (not sign) ex) (nnf sign ex')
  BinOpE t bo ex ex' -> BinOpE t (negateBinOp sign bo) (nnf sign ex) (nnf sign ex')
  IfThenElseE t cond exthen exelse -> IfThenElseE t (nnf True cond) (nnf (not sign) exthen) (nnf (not sign) exelse)
  QuantifE t q vd ex -> QuantifE t (negateQuantif sign q) vd (nnf sign ex)
  _ -> if sign then e else notExpr e


----------------------------------------------------------------------
-- Logical infrastructure: variable and index manipulation
----------------------------------------------------------------------

dropVarBy :: Int -> Var t -> Var t
dropVarBy n (LocalVar vn i) = LocalVar vn (i - n)
dropVarBy n v = v

localVarLowerThan :: Int -> Var t -> Bool
localVarLowerThan n (LocalVar vn i) = i <= n
localVarLowerThan n _ = True

-- set the index of a local variable 
index :: Int -> Var t -> Var t
index newi (LocalVar n _) = LocalVar n newi
index _ v@(GlobalVar _) = v

-- set the indices of a variable list
indexList :: [Int] -> [Var t] -> [Var t]
indexList = zipWith index

indexListFromTo :: Int -> Int -> [Var t] -> [Var t]
indexListFromTo l u = indexList (reverse [l .. u])

-- Free variables of an expression. Also LocalVar count as free variables.
fv :: Ord t => Expr t -> Set.Set (Var t)
fv ValE {} = Set.empty
fv (VarE _ v) = Set.singleton v
fv (UnaOpE _  _ e) = fv e
fv (BinOpE _  _ e1 e2) = Set.union (fv e1) (fv e2)
fv (IfThenElseE _ c e1 e2) = Set.unions [fv c, fv e1, fv e2]
fv (AppE _ f a) = Set.union (fv f) (fv a)
fv FunE {bodyOfFunE = e} = Set.map (dropVarBy 1) (Set.filter (localVarLowerThan 0) (fv e))
fv QuantifE {bodyOfExprQ = e} = Set.map (dropVarBy 1) (Set.filter (localVarLowerThan 0) (fv e))
fv (FldAccE _ e _) = fv e
fv (TupleE _ es) = Set.unions (map fv es)
fv (CastE _ _ e) = fv e
fv (ListE _ _ es) = Set.unions (map fv es)

dropVar :: Int -> Var t -> Var t
dropVar n (LocalVar vn i) = LocalVar vn (i - n)
dropVar n v = v

-- Lift variables and expressions: 
-- Increase by one all indices of bound variables with index number >= n
-- Used for pushing an expression below a quantifier

liftVar :: Int -> Var t -> Var t
liftVar n (LocalVar vn i) = if n <= i then LocalVar vn (i + 1) else LocalVar vn i
liftVar n v = v

-- lift variables with index >= n by increment inc. 
-- Thus, liftVar n v = liftVarBy n 1 v
liftVarBy :: Int -> Int -> Var t -> Var t
liftVarBy n inc (LocalVar vn i) = if n <= i then LocalVar vn (i + inc) else LocalVar vn i
liftVarBy _ _ v = v


liftExpr :: Int -> Expr t -> Expr t
liftExpr n e@(ValE t v) = e
liftExpr n (VarE t v) = VarE t (liftVar n v)
liftExpr n (UnaOpE t u et) = UnaOpE t u (liftExpr n et)
liftExpr n (BinOpE t b et1 et2) = BinOpE t b (liftExpr n et1) (liftExpr n et2)
liftExpr n (IfThenElseE t et1 et2 et3) = IfThenElseE t (liftExpr n et1) (liftExpr n et2) (liftExpr n et3)
liftExpr n (AppE t et1 et2) = AppE t (liftExpr n et1) (liftExpr n et2)
liftExpr n (FunE t v et) = FunE t v (liftExpr (n + 1) et)
liftExpr n (QuantifE t q v et) = QuantifE t q v (liftExpr (n+1) et)
liftExpr n (FldAccE t et f) = FldAccE t (liftExpr n et) f
liftExpr n (TupleE t ets) = TupleE t (map (liftExpr n) ets)
liftExpr n (CastE t tp et) = CastE t tp (liftExpr n et)
liftExpr n (ListE t lop ets) = ListE t lop (map (liftExpr n) ets)

-- Remap variables and expressions: 
-- Exchange indices of bound variables as indicated in the map
-- Used for permuting quantifiers
remapVar :: [(Int, Int)] -> Var t -> Var t
remapVar m (LocalVar vn i) = LocalVar vn (fromMaybe i (lookup i m))
remapVar _ v = v

remapExpr :: [(Int, Int)] -> Expr t -> Expr t
remapExpr m e@(ValE t v) = e
remapExpr m (VarE t v) = VarE t (remapVar m v)
remapExpr m (UnaOpE t u et) = UnaOpE t u (remapExpr m et)
remapExpr m (BinOpE t b et1 et2) = BinOpE t b (remapExpr m et1) (remapExpr m et2)
remapExpr m (IfThenElseE t et1 et2 et3) = IfThenElseE t (remapExpr m et1) (remapExpr m et2) (remapExpr m et3)
remapExpr m (AppE t et1 et2) = AppE t (remapExpr m et1) (remapExpr m et2)
remapExpr m (FunE t v et) = FunE t v (remapExpr (map (\(x, y) -> (x + 1, y + 1)) m) et)
remapExpr m (QuantifE t q v et) = QuantifE t q v (remapExpr (map (\(x, y) -> (x+1, y+1)) m) et)
remapExpr m (FldAccE t et f) = FldAccE t (remapExpr m et) f
remapExpr m (TupleE t ets) = TupleE t (map (remapExpr m) ets)
remapExpr m (CastE t tp et) = CastE t tp (remapExpr m et)
remapExpr m (ListE t lop ets) = ListE t lop (map (remapExpr m) ets)

swapQuantif :: Quantif -> Quantif
swapQuantif All = Ex
swapQuantif Ex  = All

prenexUnary :: t -> UBoolOp -> Expr t -> Expr t
prenexUnary t UBnot (QuantifE tq q v et) = QuantifE tq (swapQuantif q) v (UnaOpE t (UBool UBnot) et)
prenexUnary t u e = UnaOpE t (UBool u) e

prenexBinary :: t -> BBoolOp -> Expr t -> Expr t -> Expr t
prenexBinary t b e1 (QuantifE t2 q2 v2 e2) = QuantifE t2 q2 v2 (prenexBinary t b (liftExpr 0 e1) e2)
prenexBinary t b (QuantifE t1 q1 v1 e1) e2 =
    let q = case b of
              BBimpl -> swapQuantif q1
              _ -> q1
    in QuantifE t1 q v1 (prenexBinary t b e1 (liftExpr 0 e2))
prenexBinary t b e1 e2 = BinOpE t (BBool b) e1 e2

prenexForm :: Expr t -> Expr t
prenexForm (UnaOpE t (UBool u) et) = prenexUnary t u (prenexForm et)
prenexForm (BinOpE t (BBool b) et1 et2) = prenexBinary t b (prenexForm et1) (prenexForm et2)
prenexForm (QuantifE t q v et) = QuantifE t q v (prenexForm et)
prenexForm e = e


ruleToFormula :: Rule (Tp ()) -> Expr (Tp ())
ruleToFormula r = abstractQD All (varDeclsOfRule r) (implExpr (precondOfRule r) (postcondOfRule r))

-- abstract a Quantified expression over a list of variable Declarations
abstractQD :: Quantif -> [VarDecl (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
abstractQD q vds e = foldr (QuantifE BooleanT q) e vds

-- abstract a Quantified expression over a list of variables
abstractQ :: Quantif -> [Var (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
abstractQ q vs e
  = foldr
      (\v -> QuantifE BooleanT q (VarDecl (annotOfQVarName (nameOfVar v)) (nameOfQVarName (nameOfVar v)) (liftType (annotOfQVarName (nameOfVar v))))) e
      vs


-- abstract an expression over a list of variables to obtain a FunE
abstractF :: [Var (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
abstractF vs e
  = foldr
      (\ v re ->
        let t = annotOfQVarName (nameOfVar v)
        in FunE (FunT () t (annotOfExpr re))  (VarDecl t (nameOfQVarName (nameOfVar v)) (liftType t)) re) e
      vs


abstractFvd :: [VarDecl (Tp())] -> Expr (Tp ()) -> Expr (Tp ())
abstractFvd vds e
  = foldr
      (\ vd re -> FunE (FunT () (annotOfVarDecl vd) (annotOfExpr re)) vd re) e
      vds


-- The following decomposition functions are the (pseudo-)inverses of the abstraction functions,
-- Decomposing a FunE into a list of its variable declarations and a body 
decomposeFun :: Expr t -> ([VarDecl t], Expr t)
decomposeFun (FunE _ vd f) = let (vds, bd) = decomposeFun f in (vd:vds, bd)
decomposeFun e = ([], e)

etaExpand :: Expr (Tp()) -> Expr (Tp())
etaExpand (FunE t vd f) = FunE t vd (etaExpand f)
etaExpand e =
  case annotOfExpr e of
    FunT _ pt rt ->
      abstractFvd [VarDecl pt "etaVar" (liftType pt)]
        (etaExpand (AppE rt (liftExpr 0 e) (VarE pt (LocalVar (QVarName pt "etaVar") 0))))
    _ -> e

