-- Typing of expressions
{-
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module L4.TypeInference where

import Data.List ( elemIndex, nub )
import Data.Either (lefts)
import Data.Either.Combinators (mapLeft)


import L4.Annotation
    ( LocTypeAnnot(LocTypeAnnot), SRng (DummySRng), TypeAnnot(..), HasLoc(..), HasAnnot (getAnnot) )
import L4.Error
import L4.Syntax
import Control.Applicative (Alternative ((<|>), empty))
import L4.SyntaxManipulation ( globalVarsOfProgram )

import L4.Typing
import Control.Monad.Writer -- (Functor)
import Data.HashSet (member)



type InfClasses = [ClassName]
--  deriving (Eq, Ord, Show, Semigroup, Monoid)

{-
instance Semigroup (InfClasses t) where 
    (InfClasses xs) <> (InfClasses ys) = InfClasses (xs <> ys)
instance Monoid (InfClasses t) where 
    mempty = InfClasses []
    mappend (InfClasses xs) (InfClasses ys) = InfClasses (mappend xs ys)
-}

type InfVars = VarEnvironment
--  deriving (Eq, Ord, Show, Semigroup, Monoid)

type InfTypeConstraints = [(Tp(),Tp())]
--  deriving (Eq, Ord, Show, Semigroup, Monoid)

data InfComponents =
    InfComponents {infClasses :: InfClasses,
                   infVars :: InfVars,
                   infTypeConstraints :: InfTypeConstraints
                   }
    deriving (Eq, Ord, Show)

--  deriving (Semigroup, Monoid)
instance Semigroup InfComponents where
    (InfComponents xcls xvars xcns) <> (InfComponents ycls yvars ycns) =
        InfComponents (xcls <> ycls) (xvars <> yvars) (xcns <> ycns)
instance Monoid InfComponents where
    mempty = InfComponents [] [] []
    mappend xinfc yinfc = xinfc <> yinfc

data InferRes t = InferRes {infComponents :: InfComponents,
                            infType :: Maybe t
                            }
    deriving (Eq, Ord, Show)

instance Functor InferRes where
    fmap f (InferRes cs Nothing) = InferRes cs Nothing
    fmap f (InferRes cs (Just t)) = InferRes cs (Just (f t))

instance Applicative InferRes where
    pure t = InferRes mempty (Just t)
    (InferRes fcs ft) <*> (InferRes cs t) = InferRes (fcs <> cs) (ft <*> t)
{-
instance Monad InferRes where
    return t = InferRes mempty (Just t)
    (InferRes comps Nothing)  >>= r = InferRes comps Nothing 
    (InferRes comps (Just t)) >>= r = r t 
-}

-- declare an inferred variable by creating a new class 
-- and assigning this class to return type
declareInferredVar :: VarName -> Environment te -> InfComponents -> InferRes (Tp ())
declareInferredVar vn env icomps =
    let newClassName = genNewClassName (map nameOfClassDecl (classDeclsOfEnv env) ++ infClasses icomps)
        newClass = ClassT () newClassName 
        newDecl  = VarDecl () vn newClass in 
    InferRes (addDeclToCnstrt newDecl (addClassToCnstrt newClassName icomps)) (Just newClass)

addDeclToCnstrt :: VarDecl () -> InfComponents -> InfComponents
addDeclToCnstrt = error "not implemented"

addClassToCnstrt :: ClassName -> InfComponents -> InfComponents
addClassToCnstrt = error "not implemented"

addTpEqConstr :: Tp () -> Tp () -> InfComponents -> InfComponents
addTpEqConstr = error "not implemented"

-- generate a new name with given root which is not in strs
-- Example: genNameAwayFrom ["ab", "ab1", "ab0"] "ab" 0 = "ab2"
genNameAwayFrom :: [String] -> String -> Int -> String
genNameAwayFrom strs root n =
    let sugg = root ++ show n in
    if sugg `elem` strs
    then genNameAwayFrom strs root (n + 1)
    else sugg

{-
>>> genNameAwayFrom ["ab", "ab1", "ab0"] "ab" 0
"ab2"
-}
genNewClassName :: [ClassName] -> ClassName
genNewClassName clsnms = ClsNm (genNameAwayFrom (map stringOfClassName clsnms) "GClass" 0)

lookupExtendedEnv :: VarName -> Environment te -> InfComponents -> Maybe (Tp())
lookupExtendedEnv vn env (InfComponents _ics ivs _icns) =
    lookup vn (globalsOfEnv env) <|> lookup vn (localsOfEnv env) <|> lookup vn ivs

inferVar :: Environment te -> InfComponents -> Var t -> InferRes (Tp ())
inferVar env icomps (GlobalVar (QVarName _ vn)) =
    case lookupExtendedEnv vn env icomps of
        Nothing -> declareInferredVar vn env icomps
        Just t -> InferRes icomps (Just t)
inferVar _env _icomps (LocalVar _ _) = error "internal error: for type checking, variable should be GlobalVar"


inferExpr :: Environment te -> InfComponents -> Expr Untyped -> InferRes (Tp())
inferExpr env icomps expr = case expr of
    ValE _ c -> InferRes icomps (Just (tpConstval c))
    VarE _ v -> inferVar env icomps v
    AppE _ fe ae -> 
        let InferRes ficomps ftp = inferExpr env icomps fe in
        let InferRes aicomps atp = inferExpr env ficomps ae in
        inferAppE env aicomps ftp atp
    _ -> undefined

inferAppE :: Environment te -> InfComponents -> Maybe (Tp ()) -> Maybe (Tp ()) -> InferRes (Tp())
inferAppE _env icomps (Just (FunT _ tpar tbody)) (Just targ) =
    case targ of
        ClassT {} -> InferRes (addTpEqConstr tpar targ icomps) (Just tbody)
        _ -> InferRes icomps (Just tbody)
inferAppE env icomps (Just (ct@ClassT {})) (Just targ) = 
    let newClassName = genNewClassName (map nameOfClassDecl (classDeclsOfEnv env) ++ infClasses icomps)
        newClass = ClassT () newClassName in
    InferRes (addTpEqConstr ct (FunT () targ newClass) icomps) (Just newClass)
inferAppE _env icomps _ _ = InferRes icomps Nothing 

