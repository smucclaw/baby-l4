{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module ToGF.FromSCasp.SCasp where

import Data.Void ( Void )
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

----------------------------------------------------------------
-- Abstract syntax

-- Type that is used as an index in the GADT
data SKind = KModel | KExp | KArg | KVar | KAtom

-- The actual GADT (Generalized Abstract Datatype)
data Tree :: SKind -> * where
  MExps :: [Exp] -> Model

  EApp :: Atom -> [Arg] -> Exp

  AAtom :: Atom -> Arg
  AVar  :: Var -> Arg

  A :: String -> Atom
  V :: String -> Var

deriving instance Show (Tree k)

{-throw(C,rock) -- the string 
                -- the tree, to which the string is parsed
 EApp (A "throw") [(AVar (V "C")), (AAtom (A "rock"))]

-}
-- Shorthands for the GADT
type Exp = Tree KExp
type Var = Tree KVar
type Model = Tree KModel
type Atom = Tree KAtom
type Arg = Tree KArg

foldMapTree :: Monoid m => (forall a. Tree a -> m) -> Tree s -> m
foldMapTree f t@(MExps l_tk) = f t <> foldMap (foldMapTree f) l_tk
foldMapTree f t@(EApp tk l_tk) = f t <> foldMapTree f tk <> foldMap (foldMapTree f) l_tk
foldMapTree f t@(AAtom tk) = f t <> foldMapTree f tk
foldMapTree f t@(AVar tk) = f t <> foldMapTree f tk
foldMapTree f t = f t

dumpModels :: [Model] -> Model
dumpModels = MExps . foldMap getModel
  where
    getModel :: Model -> [Exp]
    getModel (MExps es) = es

----------------------------------------------------------------
-- Parser
-- It could be any parser, this happens to be with Megaparsec.
-- The point is the use of GADTs and type families in the abstract syntax.

type Parser = Parsec Void String

parseModel :: String -> Either String Model
parseModel = parseWError pModel ""

parseModel' :: String -> IO ()
parseModel' = parseTest pModel

parseWError :: (VisualStream s, TraversableStream s, Stream s, ShowErrorComponent e)
            => Parsec e s b -> String -> s -> Either String b
parseWError p src = either (Left . errorBundlePretty) Right . parse p src

spaceConsumer :: Parser ()
spaceConsumer = space
-- spaceConsumer = L.space
--   space1
--   (L.skipLineComment "//")
--   (L.skipBlockComment "/*" "*/")


symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser String -> Parser String
lexeme = L.lexeme spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = p `sepBy` symbol "," 

pModel :: Parser Model
pModel = MExps <$> braces (commaSeparated pExp)

pExp :: Parser Exp
pExp = EApp <$> pAtom <*> parens (commaSeparated pArg)

pVar :: Parser Var
pVar = V <$> lexeme (some upperChar)

pAtom :: Parser Atom
pAtom = fmap A $ lexeme $ some (lowerChar <|> char '_')

pArg :: Parser Arg
pArg = AAtom <$> pAtom
   <|> AVar <$> pVar
