{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore -}

module Parser (
  parseProgram
--  , parseTokens,
) where

import Lexer
import Syntax

import Prelude
import Control.Monad.Except

}

-- Entry point
%name program Program

-- Lexer structure
%tokentype { Token }


-- Parser monad
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { parseError }

-- Token Names
%token
    assert  { Token _ TokenAssert }
    class   { Token _ TokenClass }
    decl    { Token _ TokenDecl }
    defn    { Token _ TokenDefn }
    extends { Token _ TokenExtends }
    lexicon { Token _ TokenLexicon }
    rule    { Token _ TokenRule }

    Bool  { Token _ TokenBool }
    Int   { Token _ TokenInt }

    let    { Token _ TokenLet }
    in     { Token _ TokenIn }
    not    { Token _ TokenNot }
    forall { Token _ TokenForall }
    exists { Token _ TokenExists }
    if     { Token _ TokenIf }
    then   { Token _ TokenThen }
    else   { Token _ TokenElse }
    for    { Token _ TokenFor }
    true   { Token _ TokenTrue }
    false  { Token _ TokenFalse }

    '\\'  { Token _ TokenLambda }
    '->'  { Token _ TokenArrow }
    '-->' { Token _ TokenImpl }
    '||'  { Token _ TokenOr }
    '&&'  { Token _ TokenAnd }
    '='   { Token _ TokenEq }
    '<'   { Token _ TokenLt }
    '>'   { Token _ TokenGt }
    '+'   { Token _ TokenAdd }
    '-'   { Token _ TokenSub }
    '*'   { Token _ TokenMul }
    '/'   { Token _ TokenDiv }
    '%'   { Token _ TokenMod }
    '.'   { Token _ TokenDot }
    ','   { Token _ TokenComma }
    ':'   { Token _ TokenColon }
    '('   { Token _ TokenLParen }
    ')'   { Token _ TokenRParen }
    '{'   { Token _ TokenLBrace }
    '}'   { Token _ TokenRBrace }

    NUM   { Token pos (TokenNum $$) }
    VAR   { Token _ (TokenSym _) }

-- Operators
%right '->'
%left '.'
%nonassoc if then else
%right '-->'
%right '||'
%right '&&'
%left not
%nonassoc '<' '=' '>'
%left '+' '-'
%left '*' '/' '%'
%left AMINUS
%%

Program : Lexicon  ClassDecls GlobalVarDecls Rules Assertions
                                   { Program (reverse $1) (reverse $2)  (reverse $3) (reverse $4) (reverse $5) }

Lexicon :                   { [] }
        |  lexicon Mappings { $2 }

Mappings :                   {[]}
          | Mappings Mapping {$2 : $1 }
Mapping : VAR '->' VAR { Mapping (tokenRange $1 $3) (tokenSym $1) (tokenSym $3) }
ClassDecls :                       { [] }
           | ClassDecls ClassDecl  { $2 : $1 }
ClassDecl : class VAR ClassDef     { ClassDecl (ClsNm $ tokenSym $2) $3 }

ClassDef :   Fields                { ClassDef (Just (ClsNm "Class")) (reverse $1) }
         |   extends VAR Fields    { ClassDef (Just (ClsNm $ tokenSym $2)) (reverse $3) }

Fields  :                          { [] }
        | '{' FieldDecls '}'       { $2 }
FieldDecls :                       { [] }
           | FieldDecls FieldDecl  { $2 : $1 }

FieldDecl : VAR ':' Tp             { FieldDecl (FldNm $ tokenSym $1) $3 }

GlobalVarDecls :                         { [] }
         | GlobalVarDecls GlobalVarDecl  { $2 : $1 }

GlobalVarDecl : decl VAR ':' Tp          { VarDecl (tokenSym $2) $4 }

VarDeclsCommaSep :  VarDecl              { [$1] }
         | VarDeclsCommaSep  ',' VarDecl { $3 : $1 }

VarDecl : VAR ':' Tp                     { VarDecl (tokenSym $1) $3 }


Assertions :                       { [] }
           | Assertions Assertion  { $2 : $1 }
Assertion : assert Expr            { Assertion $2 }

-- Atomic type
ATp  : Bool                       { BoolT }
     | Int                        { IntT }
     | VAR                        { ClassT (ClsNm $ tokenSym $1) }
     | '(' TpsCommaSep ')'        { let tcs = $2 in if length tcs == 1 then head tcs else TupleT (reverse tcs) }

TpsCommaSep :                      { [] }
            | Tp                   { [$1] }
            | TpsCommaSep ',' Tp   { $3 : $1 }

Tp   : ATp                        { $1 }
     | Tp '->' Tp                 { FunT $1 $3 }


Pattern : VAR                      { VarP $ tokenSym $1 }
    | '(' VarsCommaSep ')'         { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

VarsCommaSep :                      { [] }
            | VAR                   { [tokenSym $1] }
            | VarsCommaSep ',' VAR  { tokenSym $3 : $1 }

Expr : '\\' Pattern ':' ATp '->' Expr  { FunE (tokenRange $1 $6) () $2 $4 $6 }
     | forall VAR ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) () All (tokenSym $2) $4 $6 }
     | exists VAR ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) () Ex (tokenSym $2) $4 $6 }
     | Expr '-->' Expr             { BinOpE (tokenRange $1 $3) () (BBool BBimpl) $1 $3 }
     | Expr '||' Expr              { BinOpE (tokenRange $1 $3) () (BBool BBor) $1 $3 }
     | Expr '&&' Expr              { BinOpE (tokenRange $1 $3) () (BBool BBand) $1 $3 }
     | if Expr then Expr else Expr { IfThenElseE (tokenRange $1 $6) () $2 $4 $6 }
     | not Expr                    { UnaOpE (tokenRange $1 $2) () (UBool UBneg) $2 }
     | Expr '<' Expr               { BinOpE (tokenRange $1 $3) () (BCompar BClt) $1 $3 }
     | Expr '>' Expr               { BinOpE (tokenRange $1 $3) () (BCompar BCgt) $1 $3 }
     | Expr '=' Expr               { BinOpE (tokenRange $1 $3) () (BCompar BCeq) $1 $3 }
     | Expr '+' Expr               { BinOpE (tokenRange $1 $3) () (BArith BAadd) $1 $3 }
     | Expr '-' Expr               { BinOpE (tokenRange $1 $3) () (BArith BAsub) $1 $3 }
     | '-' Expr %prec AMINUS       { UnaOpE (tokenRange $1 $2) () (UArith UAminus) $2 }
     | Expr '*' Expr               { BinOpE (tokenRange $1 $3) () (BArith BAmul) $1 $3 }
     | Expr '/' Expr               { BinOpE (tokenRange $1 $3) () (BArith BAdiv) $1 $3 }
     | Expr '%' Expr               { BinOpE (tokenRange $1 $3) () (BArith BAmod) $1 $3 }
     | App                         { $1 }

App : App Acc                     { AppE (tokenRange $1 $2) () $1 $2 }
    | Acc                          { $1 }

-- field access
Acc : Acc '.' VAR                  { FldAccE (tokenRange $1 $3) () $1 (FldNm $ tokenSym $3) }
    | Atom                         { $1 }

Atom : '(' ExprsCommaSep ')'       { let ecs = $2 in if length ecs == 1 then head ecs else TupleE (tokenRange $1 $3) () (reverse ecs) }
     | NUM                         { ValE (pos) () (IntV $1) }
     | VAR                         { VarE (tokenPos $1) () (GlobalVar $ tokenSym $1) }
     | true                        { ValE (tokenPos $1) () (BoolV True) }
     | false                       { ValE (tokenPos $1) () (BoolV False) }

ExprsCommaSep :                      { [] }
            | Expr                   { [$1] }
            | ExprsCommaSep ',' Expr  { $3 : $1 }


Rules  :                       { [] }
       | Rules Rule            { $2 : $1}
Rule:  RuleName RuleVarDecls RulePrecond RuleConcl { Rule $1 $2 $3 $4 }

RuleName: rule '<' VAR '>'     { tokenSym $3 }

RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }

{

tokenSym (Token _ (TokenSym sym)) = sym

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

-- parseError :: [Token] -> Except String a
-- parseError (l:ls) = throwError (show l)
-- parseError [] = throwError "Unexpected end of Input"

parseProgram :: FilePath -> String -> Either Err (Program (Maybe ClassName) _)
parseProgram = runAlex' program

-- parseProgram:: String -> Either String (Program (Maybe ClassName) ())
-- parseProgram input = runExcept $ do
--   tokenStream <- scanTokens input
--   program tokenStream

-- still needed ???
-- parseTokens :: String -> Either String [Token]
-- parseTokens = runExcept . scanTokens

}
