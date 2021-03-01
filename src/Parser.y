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
%lexer { lexwrap } { Token _ _ TokenEOF }
%error { parseError }

-- Token Names
%token
    assert  { Token _ _ TokenAssert }
    class   { Token _ _ TokenClass }
    decl    { Token _ _ TokenDecl }
    defn    { Token _ _ TokenDefn }
    extends { Token _ _ TokenExtends }
    lexicon { Token _ _ TokenLexicon }
    rule    { Token _ _ TokenRule }

    Bool  { Token _ _ TokenBool }
    Int   { Token _ _ TokenInt }

    let    { Token _ _ TokenLet }
    in     { Token _ _ TokenIn }
    not    { Token _ _ TokenNot }
    forall { Token _ _ TokenForall }
    exists { Token _ _ TokenExists }
    if     { Token _ _ TokenIf }
    then   { Token _ _ TokenThen }
    else   { Token _ _ TokenElse }
    for    { Token _ _ TokenFor }
    true   { Token _ _ TokenTrue }
    false  { Token _ _ TokenFalse }

    '\\'  { Token _ _ TokenLambda }
    '->'  { Token _ _ TokenArrow }
    '-->' { Token _ _ TokenImpl }
    '||'  { Token _ _ TokenOr }
    '&&'  { Token _ _ TokenAnd }
    '='   { Token _ _ TokenEq }
    '<'   { Token _ _ TokenLt }
    '>'   { Token _ _ TokenGt }
    '+'   { Token _ _ TokenAdd }
    '-'   { Token _ _ TokenSub }
    '*'   { Token _ _ TokenMul }
    '/'   { Token _ _ TokenDiv }
    '%'   { Token _ _ TokenMod }
    '.'   { Token _ _ TokenDot }
    ','   { Token _ _ TokenComma }
    ':'   { Token _ _ TokenColon }
    '('   { Token _ _ TokenLParen }
    ')'   { Token _ _ TokenRParen }
    '{'   { Token _ _ TokenLBrace }
    '}'   { Token _ _ TokenRBrace }

    NUM   { Token _ _ (TokenNum $$) }
    VAR   { Token _ _ (TokenSym $$) }

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
Lexicon : lexicon Mappings { $2 }
Mappings :                   {[]}
          | Mappings Mapping {$2 : $1 }
Mapping : VAR '->' VAR { Mapping $1 $3 }
ClassDecls :                       { [] }
           | ClassDecls ClassDecl  { $2 : $1 }
ClassDecl : class VAR ClassDef     { ClassDecl (ClsNm $2) $3 }

ClassDef :  '{' FieldDecls '}'     { ClassDef (Just (ClsNm "Object")) (reverse $2) }
         |   extends VAR '{' FieldDecls '}'
                                   { ClassDef (Just (ClsNm $2)) (reverse $4) }
FieldDecls :                       { [] }
           | FieldDecls FieldDecl  { $2 : $1 }

FieldDecl : VAR ':' Tp             { FieldDecl (FldNm $1) $3 }

GlobalVarDecls :                         { [] }
         | GlobalVarDecls GlobalVarDecl  { $2 : $1 }

GlobalVarDecl : decl VAR ':' Tp          { VarDecl $2 $4 }

VarDeclsCommaSep :  VarDecl              { [$1] }
         | VarDeclsCommaSep  ',' VarDecl { $3 : $1 }

VarDecl : VAR ':' Tp                     { VarDecl $1 $3 }


Assertions :                       { [] }
           | Assertions Assertion  { $2 : $1 }
Assertion : assert Expr            { Assertion $2 }

-- Atomic type
ATp   : Bool                      { BoolT }
     | Int                        { IntT }
     | VAR                        { ClassT (ClsNm $1) }
     | '(' TpsCommaSep ')'        { let tcs = $2 in if length tcs == 1 then head tcs else TupleT (reverse tcs) }

TpsCommaSep :                      { [] }
            | Tp                   { [$1] }
            | TpsCommaSep ',' Tp   { $3 : $1 }

Tp   : ATp                        { $1 }
     | Tp '->' Tp                 { FunT $1 $3 }


Pattern : VAR                      { VarP $1 }
    | '(' VarsCommaSep ')'         { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

VarsCommaSep :                      { [] }
            | VAR                   { [$1] }
            | VarsCommaSep ',' VAR  { $3 : $1 }

Expr : '\\' Pattern ':' ATp '->' Expr  { FunE () $2 $4 $6 }
     | forall VAR ':' Tp '.' Expr      { QuantifE () All $2 $4 $6 }
     | exists VAR ':' Tp '.' Expr      { QuantifE () Ex $2 $4 $6 }
     | Expr '-->' Expr             { BinOpE () (BBool BBimpl) $1 $3 }
     | Expr '||' Expr              { BinOpE () (BBool BBor) $1 $3 }
     | Expr '&&' Expr              { BinOpE () (BBool BBand) $1 $3 }
     | if Expr then Expr else Expr { IfThenElseE () $2 $4 $6 }
     | not Expr                    { UnaOpE () (UBool UBneg) $2 }
     | Expr '<' Expr               { BinOpE () (BCompar BClt) $1 $3 }
     | Expr '>' Expr               { BinOpE () (BCompar BCgt) $1 $3 }
     | Expr '=' Expr               { BinOpE () (BCompar BCeq) $1 $3 }
     | Expr '+' Expr               { BinOpE () (BArith BAadd) $1 $3 }
     | Expr '-' Expr               { BinOpE () (BArith BAsub) $1 $3 }
     | '-' Expr %prec AMINUS       { UnaOpE () (UArith UAminus) $2 }
     | Expr '*' Expr               { BinOpE () (BArith BAmul) $1 $3 }
     | Expr '/' Expr               { BinOpE () (BArith BAdiv) $1 $3 }
     | Expr '%' Expr               { BinOpE () (BArith BAmod) $1 $3 }
     | App                         { $1 }

App : App Acc                     { AppE () $1 $2 }
    | Acc                          { $1 }

-- field access
Acc : Acc '.' VAR                  { FldAccE () $1 (FldNm $3) }
    | Atom                         { $1 }

Atom : '(' ExprsCommaSep ')'       { let ecs = $2 in if length ecs == 1 then head ecs else TupleE () (reverse ecs) }
     | NUM                         { ValE () (IntV $1) }
     | VAR                         { VarE () (GlobalVar $1) }
     | true                        { ValE () (BoolV True) }
     | false                       { ValE () (BoolV False) }

ExprsCommaSep :                      { [] }
            | Expr                   { [$1] }
            | ExprsCommaSep ',' Expr  { $3 : $1 }


Rules  :                       { [] }
       | Rules Rule            { $2 : $1}
Rule:  RuleName RuleVarDecls RulePrecond RuleConcl { Rule $1 $2 $3 $4 }

RuleName: rule '<' VAR '>'     { $3 }

RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }

-- Annotations for GF
Annot : '(' NUM ')'                 { GFAnnot $2 }


{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p _ t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

-- parseError :: [Token] -> Except String a
-- parseError (l:ls) = throwError (show l)
-- parseError [] = throwError "Unexpected end of Input"

parseProgram :: FilePath -> String -> Either Err (Program (Maybe ClassName) ())
parseProgram = runAlex' program

-- parseProgram:: String -> Either String (Program (Maybe ClassName) ())
-- parseProgram input = runExcept $ do
--   tokenStream <- scanTokens input
--   program tokenStream

-- still needed ???
-- parseTokens :: String -> Either String [Token]
-- parseTokens = runExcept . scanTokens

}
