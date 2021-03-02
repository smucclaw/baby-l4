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


import Control.Applicative as App (Applicative (..))


import Data.Word (Word8)

import Data.Char (ord)
import qualified Data.Bits
import qualified Language.LSP.Types            as J

import Syntax (HasAnn(..))


}

-- %wrapper "monadUserState"

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

--------------------------------------------------------------------------------
-- Copied from https://github.com/simonmar/alex/blob/master/data/AlexWrappers.hs
-- and modified to use a custom error type
--------------------------------------------------------------------------------

-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.



-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, s))



-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)


data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location

        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],





        alex_scd :: !Int        -- the current startcode

      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program

    }

-- Compile with -funbox-strict-fields for best results!


runAlex :: String -> Alex a -> Either Err a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bytes = [],





                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',

                        alex_ust = alexInitUserState,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either Err (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))





alexSetInput :: AlexInput -> Alex ()

alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of







                  state__@(AlexState{}) -> Right (state__, ())

alexError :: Err -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())


alexMonadScan = do

  inp__ <- alexGetInput



  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError (pos@(AlexPn _ line column),_,_,_) -> alexError $ Err (alex2lspRng pos 1) $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan

    AlexToken inp__' len action -> do



        alexSetInput inp__'
        action (ignorePendingBytes inp__) len

-- -----------------------------------------------------------------------------
-- Useful token actions


type AlexAction result = AlexInput -> Int -> Alex result




-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len


token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)

---------------------------------
-- End copied AlexWrapper code --
---------------------------------

data Err = Err { epos :: J.Range , msg :: String }
  deriving (Show)

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

type Token = TokenAnn J.Range

data TokenAnn a = Token { tokenPos:: a, tokenKind :: TokenKind }
  deriving (Show, Functor)

getTokenKind (Token _ k) = k

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
  return $ Token (alex2lspRng p 0) TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenKind) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token (alex2lspRng p i) (f (take i s))

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
        alexError' (alex2lspRng p 1) ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: J.Range -> String -> Alex a
alexError' p@(J.Range (J.Position l c) _) msg = do
  fp <- getFilePath
  alexError $ Err p (fp ++ ":" ++ show (l+1) ++ ":" ++ show (c+1) ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either Err a
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

isEof x = tokenKind x == TokenEOF

scanTokens :: FilePath -> String -> Either Err [Token]
scanTokens = runAlex' allTokens
  where
    allTokens = repeatUntil isEof alexMonadScan'

scanFile :: FilePath -> IO (Either Err [Token])
scanFile fname = scanTokens fname <$> readFile fname

alex2lspRng :: AlexPosn -> Int -> J.Range
alex2lspRng tokenPos tokenLen = J.Range startPos endPos
  where
    startPos = aposToPos tokenPos
    endPos = offset tokenLen startPos

aposToPos :: AlexPosn -> J.Position
aposToPos (AlexPn _ l c) = J.Position (l - 1) (c - 1)

-- horizontal offset, assuming tokens do not extend over several lines
offset :: Int -> J.Position -> J.Position
offset n (J.Position l c) = J.Position l (c + n)

coordFromTo :: J.Range -> J.Range -> J.Range
coordFromTo (J.Range f1 t1) (J.Range f2 t2) = J.Range f1 t2

tokenRange :: (HasAnn f, HasAnn g) => f J.Range -> g J.Range -> J.Range
tokenRange a b = coordFromTo (getAnn a) (getAnn b)

startOf :: J.Range -> J.Position
startOf (J.Range s _) = s

instance HasAnn TokenAnn where
  getAnn = tokenPos

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
