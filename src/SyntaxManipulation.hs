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
import Syntax
import Typing (eraseAnn)


----------------------------------------------------------------------
-- Operations on types, checking term properties
----------------------------------------------------------------------

liftType :: Tp () -> Tp (Tp ())
liftType t = KindT <$ t

forceArgTp :: Tp () -> Tp ()
forceArgTp (FunT _ ftp atp) = atp
forceArgTp _ = ErrT

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
mkIntConst f = ValE integerT (IntV f)

mkFloatConst :: Float -> Expr (Tp())
mkFloatConst f = ValE floatT (FloatV f)

mkVarE :: Var t -> Expr t
mkVarE v = VarE {annotOfExpr =  annotOfQVarName (nameOfVar v), varOfExprVarE = v}

-- decomposes a function type T1 -> T2 ... -> Tn -> Tres
-- into ([T1, ... Tn], Tres)
spine :: [Tp t] -> Tp t -> ([Tp t], Tp t)
spine acc (FunT _ t1 t2) = spine (t1:acc) t2
spine acc t = (reverse acc, t)

-- decompose application (f a1 .. an) into (f, [a1 .. an])
appToFunArgs :: [Expr t] -> Expr t -> (Expr t, [Expr t])
appToFunArgs acc (AppE _ f a) = appToFunArgs (a:acc) f
appToFunArgs acc t = (t, acc)

-- compose (f, [a1 .. an]) to (f a1 .. an)
funArgsToApp :: Expr (Tp ()) -> [Expr (Tp ())] -> Expr (Tp ())
funArgsToApp = foldl (\ f -> AppE (forceArgTp (annotOfExpr f)) f)

applyVars :: Var (Tp()) -> [Var (Tp())] -> Expr (Tp())
applyVars f args = funArgsToApp (mkVarE f) (map mkVarE args)

notExpr :: Expr (Tp ()) -> Expr (Tp())
notExpr = UnaOpE booleanT (UBool UBnot)

conjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
conjExpr = BinOpE booleanT (BBool BBand)

disjExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
disjExpr = BinOpE booleanT (BBool BBor)

implExpr :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
implExpr = BinOpE booleanT (BBool BBimpl)

conjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
conjsExpr [] = trueV
conjsExpr [e] = e
conjsExpr (e:es) = conjExpr e (conjsExpr es)

disjsExpr :: [Expr (Tp ())] -> Expr (Tp ())
disjsExpr [] = falseV
disjsExpr [e] = e
disjsExpr (e:es) = disjExpr e (disjsExpr es)

eqExpr  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
eqExpr = BinOpE booleanT (BCompar BCeq)

gteExpr  :: Expr (Tp ()) -> Expr (Tp ()) -> Expr (Tp ())
gteExpr = BinOpE booleanT (BCompar BCgte)

mkEq :: Var (Tp()) -> Var (Tp()) -> Expr (Tp())
mkEq v1 v2 = eqExpr (mkVarE v1) (mkVarE v2)


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
abstractQD q vds e = foldr (QuantifE booleanT q) e vds

-- abstract a Quantified expression over a list of variables
abstractQ :: Quantif -> [Var (Tp ())] -> Expr (Tp ()) -> Expr (Tp ())
abstractQ q vs e
  = foldr
      (\v -> QuantifE booleanT q (VarDecl (annotOfQVarName (nameOfVar v)) (nameOfQVarName (nameOfVar v)) (liftType (annotOfQVarName (nameOfVar v))))) e
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
