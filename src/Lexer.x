{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS -w  #-}
{- HLINT ignore -}

module Lexer
-- (
--   Token(..)
--   -- scanTokens,
--   , AlexPosn(..)
--   , TokenKind(..)
--   , unLex
--   , Alex(..)
--   , runAlex'
--   , alexMonadScan'
--   , alexError'
--   , module Lexer
-- )
 where

import Prelude hiding (lex)
import Control.Monad.Except

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  -- Structuring elements of an L4 file

  assert                        { lex' TokenAssert }
  class                         { lex' TokenClass }
  decl                          { lex' TokenDecl }
  defn                          { lex' TokenDefn }
  extends                       { lex' TokenExtends }
  lexicon                       { lex' TokenLexicon }
  rule                          { lex' TokenRule }

  -- Types
  Bool                          { lex' TokenBool }
  Int                           { lex' TokenInt }

  -- Expressions
  let                           { lex' TokenLet }
  in                            { lex' TokenIn }
  not                           { lex' TokenNot }
  forall                        { lex' TokenForall }
  exists                        { lex' TokenExists }
  if                            { lex' TokenIf }
  then                          { lex' TokenThen }
  else                          { lex' TokenElse }
  for                           { lex' TokenFor }
  True                          { lex' TokenTrue }
  False                         { lex' TokenFalse }

  -- Symbols
  "->"                          { lex' TokenArrow }
  \\                            { lex' TokenLambda }
  "-->"                         { lex' TokenImpl }
  "||"                          { lex' TokenOr }
  "&&"                          { lex' TokenAnd }
  \=                            { lex' TokenEq }
  \<                            { lex' TokenLt }
  \>                            { lex' TokenGt }
  [\+]                          { lex' TokenAdd }
  [\-]                          { lex' TokenSub }
  [\*]                          { lex' TokenMul }
  "/"                           { lex' TokenDiv }
  "%"                           { lex' TokenMod }
  \.                            { lex' TokenDot }
  \,                            { lex' TokenComma }
  \:                            { lex' TokenColon }
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  \{                            { lex' TokenLBrace }
  \}                            { lex' TokenRBrace }

  -- Numbers and identifiers
  $digit+                       { lex (TokenNum . read) }
  $alpha [$alpha $digit \_ \']* { lex TokenSym }


{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

type Token = TokenAnn AlexPosn

data TokenAnn a = Token { tokenPos:: a, tokenLen :: Int, tokenKind :: TokenKind }
  deriving (Show, Functor)

getTokenKind (Token _ _ k) = k

data TokenKind
  = TokenAssert
  | TokenClass
  | TokenDecl
  | TokenDefn
  | TokenExtends
  | TokenLexicon
  | TokenRule

  | TokenBool
  | TokenInt

  | TokenLet
  | TokenIn
  | TokenNot
  | TokenForall
  | TokenExists
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFor
  | TokenTrue
  | TokenFalse

  | TokenLambda
  | TokenArrow
  | TokenImpl
  | TokenOr
  | TokenAnd
  | TokenEq
  | TokenLt
  | TokenGt
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenMod
  | TokenDot
  | TokenComma
  | TokenColon
  | TokenLBrace
  | TokenRBrace
  | TokenLParen
  | TokenRParen
  | TokenEOF

  | TokenNum Integer
  | TokenSym String
  deriving (Eq,Show)

-- For nice parser error messages.
unLex :: TokenKind -> String
unLex TokenAssert  = "assert"
unLex TokenClass   = "class"
unLex TokenDecl    = "decl"
unLex TokenDefn    = "defn"
unLex TokenExtends = "extends"
unLex TokenLexicon = "lexicon"
unLex TokenRule    = "rule"
unLex TokenBool    = "Bool"
unLex TokenInt     = "Int"
unLex TokenLet     = "let"
unLex TokenIn      = "in"
unLex TokenNot     = "not"
unLex TokenForall  = "forall"
unLex TokenExists  = "exists"
unLex TokenIf      = "if"
unLex TokenThen    = "then"
unLex TokenElse    = "else"
unLex TokenFor     = "for"
unLex TokenTrue    = "True"
unLex TokenFalse   = "False"
unLex TokenArrow   = "->"
unLex TokenLambda  = "\\"
unLex TokenImpl    = "-->"
unLex TokenOr      = "||"
unLex TokenAnd     = "&&"
unLex TokenEq      = "="
unLex TokenLt      = "<"
unLex TokenGt      = ">"
unLex TokenAdd     = "+"
unLex TokenSub     = "-"
unLex TokenMul     = "*"
unLex TokenDiv     = "/"
unLex TokenMod     = "%"
unLex TokenDot     = "."
unLex TokenComma   = ","
unLex TokenColon   = ":"
unLex TokenLParen  = "("
unLex TokenRParen  = ")"
unLex TokenLBrace  = "{"
unLex TokenRBrace  = "}"
unLex TokenEOF     = "<EOF>"
unLex (TokenNum i) = show i
unLex (TokenSym s) = show s

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p 0 TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenKind) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p i (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenKind -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)

-- repeatUntil (== "\EOT") $ do {x <- getLine; print x; return x }
repeatUntil :: Monad m => (a -> Bool) -> m a -> m [a]
repeatUntil test single = single >>= go
  where
    go x | test x = pure [x]
    go x = do
      y <- single
      ys <- go y
      return (x:ys)

isEof (Token _ _ TokenEOF) = True
isEof _ = False

scanTokens :: FilePath -> String -> Either String [Token]
scanTokens = runAlex' allTokens
  where
    allTokens = repeatUntil isEof alexMonadScan'

scanFile :: FilePath -> IO (Either String [Token])
scanFile fname = scanTokens fname <$> readFile fname

-- This might be useful for looking up token locations:
-- http://hackage.haskell.org/package/IntervalMap
-- or possibly:
-- http://hackage.haskell.org/package/SegmentTree
-- See: https://stackoverflow.com/questions/3893281/haskell-range-map-library


-- matchesPos :: Int -> Int -> Token -> Bool
-- matchesPos line col (Token (AlexPn _ l c) len _) =
--   line == l -- && col == c
--   && col >= c && col < c + len

-- >>> Right x <- scanFile "l4/mini.l4"
-- >>> mapM_ print x

-- scanTokens :: String -> Except String [Token]
-- scanTokens str = go ('\n',[],str) where
--   go inp@(_,_bs,str) =
--     case alexScan inp 0 of
--      AlexEOF -> return []
--      AlexError _ -> throwError "Invalid lexeme."
--      AlexSkip  inp' len     -> go inp'
--      AlexToken inp' len act -> do
--       res <- go inp'
--       let rest = act (take len str)
--       return (rest : res)

}
