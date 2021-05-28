{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore -}

module Parser (
  parseProgram
--  , parseTokens,
) where

import Lexer
import Annotation
import KeyValueMap
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
%lexer { lexwrap } { L _ TokenEOF }
%error { parseError }

-- Token Names
%token
    assert  { L _ TokenAssert }
    class   { L _ TokenClass }
    decl    { L _ TokenDecl }
    defn    { L _ TokenDefn }
    extends { L _ TokenExtends }
    lexicon { L _ TokenLexicon }
    fact    { L _ TokenFact }
    rule    { L _ TokenRule }
    derivable { L _ TokenDerivable }

    Bool  { L _ TokenBool }
    Int   { L _ TokenInt }

    let    { L _ TokenLet }
    in     { L _ TokenIn }
    not    { L _ TokenNot }
    forall { L _ TokenForall }
    exists { L _ TokenExists }
    if     { L _ TokenIf }
    then   { L _ TokenThen }
    else   { L _ TokenElse }
    for    { L _ TokenFor }
    true   { L _ TokenTrue }
    false  { L _ TokenFalse }

    '\\'  { L _ TokenLambda }
    '->'  { L _ TokenArrow }
    '-->' { L _ TokenImpl }
    '||'  { L _ TokenOr }
    '&&'  { L _ TokenAnd }
    '='   { L _ TokenEq }
    '<'   { L _ TokenLt }
    '<='  { L _ TokenLte }
    '>'   { L _ TokenGt }
    '>='  { L _ TokenGte }
    '+'   { L _ TokenAdd }
    '-'   { L _ TokenSub }
    '*'   { L _ TokenMul }
    '/'   { L _ TokenDiv }
    '%'   { L _ TokenMod }
    '.'   { L _ TokenDot }
    ','   { L _ TokenComma }
    ':'   { L _ TokenColon }
    '('   { L _ TokenLParen }
    ')'   { L _ TokenRParen }
    '{'   { L _ TokenLBrace }
    '}'   { L _ TokenRBrace }

    NUM   { L pos (TokenNum $$) }
    VAR   { L _ (TokenSym _) }
    STRLIT { L _ (TokenStringLit _)}
    STR   { L _ (TokenString _) }

-- Operators
%right '->'
%left '.'
%nonassoc if then else
%right '-->'
%right '||'
%right '&&'
%left not
%nonassoc '<' '<=' '=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left AMINUS
%%

Program : Lexicon ClassDecls GlobalVarDecls Rules Assertions
                                   { Program (tokenRangeList [getLoc $1, getLoc $2, getLoc $3, getLoc $4, getLoc $5]) (reverse $ unLoc $1) (reverse $2)  (reverse $3) (reverse $4) (reverse $5) }

Lexicon :                   { L (DummySRng "No lexicon") [] }
        |  lexicon Mappings { L (tokenRangeList [getLoc $1, getLoc $2]) $2 }

Mappings :  Mapping          { [$1] }
          | Mappings Mapping { $2 : $1 }
Mapping : VAR '->' STRLIT { Mapping (tokenRange $1 $3) (tokenSym $1) (tokenStringLit $3) }

ClassDecls :                       { [] }
           | ClassDecls ClassDecl  { $2 : $1 }

ClassDecl : class VAR ClassDef     { case snd $3 of
                                       -- ClassDef is empty, so take token range of 'class VAR'
                                       Nothing ->  if tokenSym $2 == "Object"
                                                   -- special treatment: create Object class without superclass
                                                   then ClassDecl (tokenRange $1 $2) (ClsNm $ tokenSym $2) (ClassDef [] [])
                                                   -- take default class created in first component of $3
                                                   else ClassDecl (tokenRange $1 $2) (ClsNm $ tokenSym $2) (fst $3)
                                       -- ClassDef is non-empty, but the data type ClassDef has no position annotation,
                                       -- so retrieve position info from production ClassDef
                                       Just rng -> ClassDecl (coordFromTo (getLoc $1) rng) (ClsNm $ tokenSym $2) (fst $3) }

ClassDef :   Fields                { (ClassDef [ClsNm "Class"] (reverse (fst $1)), (snd $1)) }
         |   extends VAR Fields    { case snd $3 of
                 Nothing -> (ClassDef [ClsNm $ tokenSym $2] (reverse (fst $3)), Just (tokenRange $1 $2))
                 Just rng -> (ClassDef [ClsNm $ tokenSym $2] (reverse (fst $3)), Just (coordFromTo (getLoc $1) rng )) }

Fields  :                          { ([], Nothing) }
        | '{' FieldDecls '}'       { ($2, Just (tokenRange $1 $3)) }

FieldDecls :                       { [] }
           | FieldDecls FieldDecl  { $2 : $1 }

FieldDecl : VAR ':' Tp             { FieldDecl (tokenRange $1 $3) (FldNm $ tokenSym $1) (unLoc $3) }

GlobalVarDecls :                         { [] }
         | GlobalVarDecls GlobalVarDecl  { $2 : $1 }

GlobalVarDecl : decl VAR ':' Tp          { VarDecl (tokenRange $1 $4) (tokenSym $2) (unLoc $4) }

VarDeclsCommaSep :  VarDecl              { [$1] }
         | VarDeclsCommaSep  ',' VarDecl { $3 : $1 }

VarDecl : VAR ':' Tp                     { VarDecl (tokenRange $1 $3) (tokenSym $1) (unLoc $3) }


Assertions :                       { [] }
           | Assertions Assertion  { $2 : $1 }
Assertion : assert KVMap Expr      { Assertion (tokenRange $1 $3) $2 $3 }

-- Atomic type
-- Used to resolve ambigouity of     \x : A -> B -> x
-- and force the use of parenthesis: \x : (A -> B) -> x
ATp  : Bool                       { L (getLoc $1) booleanT }
     | Int                        { L (getLoc $1) integerT }
     | VAR                        { L (getLoc $1) $ ClassT (ClsNm $ tokenSym $1) }
     | '(' TpsCommaSep ')'        { L (getLoc $2) $ case $2 of [t] -> unLoc t; tcs -> TupleT (map unLoc $ reverse tcs) }

TpsCommaSep :                      { [] }
            | Tp                   { [$1] }
            | TpsCommaSep ',' Tp   { $3 : $1 }

Tp   : ATp                        { $1 }
     | Tp '->' Tp                 { L (tokenRange $1 $3) $ FunT (unLoc $1) (unLoc $3) }


Pattern : VAR                      { VarP $ tokenSym $1 }
    | '(' VarsCommaSep ')'         { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

VarsCommaSep :                      { [] }
            | VAR                   { [tokenSym $1] }
            | VarsCommaSep ',' VAR  { tokenSym $3 : $1 }

Expr : '\\' Pattern ':' ATp '->' Expr  { FunE (tokenRange $1 $6) $2 (unLoc $4) $6 }
     | forall VAR ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) All (tokenSym $2) (unLoc $4) $6 }
     | exists VAR ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) Ex  (tokenSym $2) (unLoc $4) $6 }
     | Expr '-->' Expr             { BinOpE (tokenRange $1 $3) (BBool BBimpl) $1 $3 }
     | Expr '||' Expr              { BinOpE (tokenRange $1 $3) (BBool BBor) $1 $3 }
     | Expr '&&' Expr              { BinOpE (tokenRange $1 $3) (BBool BBand) $1 $3 }
     | if Expr then Expr else Expr { IfThenElseE (tokenRange $1 $6) $2 $4 $6 }
     | not Expr                    { UnaOpE (tokenRange $1 $2) (UBool UBnot) $2 }
     | not derivable VAR           { NotDeriv (tokenRange $1 $3) True (VarE (tokenPos $3) (GlobalVar $ tokenSym $3)) }
     | not derivable not VAR       { NotDeriv (tokenRange $1 $4) False (VarE (tokenPos $4) (GlobalVar $ tokenSym $4)) }
     | not derivable VAR Atom      { NotDeriv (tokenRange $1 $4) True (AppE (tokenRange $3 $4)  (VarE (tokenPos $3) (GlobalVar $ tokenSym $3)) $4) }
     | not derivable not VAR Atom  { NotDeriv (tokenRange $1 $5) False (AppE (tokenRange $4 $5) (VarE (tokenPos $4) (GlobalVar $ tokenSym $4)) $5) }
     | Expr '<' Expr               { BinOpE (tokenRange $1 $3) (BCompar BClt) $1 $3 }
     | Expr '<=' Expr              { BinOpE (tokenRange $1 $3) (BCompar BClte) $1 $3 }
     | Expr '>' Expr               { BinOpE (tokenRange $1 $3) (BCompar BCgt) $1 $3 }
     | Expr '>=' Expr              { BinOpE (tokenRange $1 $3) (BCompar BCgte) $1 $3 }
     | Expr '=' Expr               { BinOpE (tokenRange $1 $3) (BCompar BCeq) $1 $3 }
     | Expr '+' Expr               { BinOpE (tokenRange $1 $3) (BArith BAadd) $1 $3 }
     | Expr '-' Expr               { BinOpE (tokenRange $1 $3) (BArith BAsub) $1 $3 }
     | '-' Expr %prec AMINUS       { UnaOpE (tokenRange $1 $2) (UArith UAminus) $2 }
     | Expr '*' Expr               { BinOpE (tokenRange $1 $3) (BArith BAmul) $1 $3 }
     | Expr '/' Expr               { BinOpE (tokenRange $1 $3) (BArith BAdiv) $1 $3 }
     | Expr '%' Expr               { BinOpE (tokenRange $1 $3) (BArith BAmod) $1 $3 }
     | App                         { $1 }

App : App Acc                     { AppE (tokenRange $1 $2) $1 $2 }
    | Acc                          { $1 }

-- field access
Acc : Acc '.' VAR                  { FldAccE (tokenRange $1 $3) $1 (FldNm $ tokenSym $3) }
    | Atom                         { $1 }

Atom : '(' ExprsCommaSep ')'       { let ecs = $2
                                     in
                                        if length ecs == 1
                                        then updAnnotOfExpr (const (tokenRange $1 $3)) (head ecs)
                                        else TupleE (tokenRange $1 $3) (reverse ecs) }
     | NUM                         { ValE (pos) (IntV $1) }
     | STR                         { ValE (tokenPos $1) (StringV (tokenString $1)) }
     | VAR                         { VarE (tokenPos $1) (GlobalVar $ tokenSym $1) }
     | true                        { ValE (tokenPos $1) (BoolV True) }
     | false                       { ValE (tokenPos $1) (BoolV False) }

ExprsCommaSep :                      { [] }
            | Expr                   { [$1] }
            | ExprsCommaSep ',' Expr  { $3 : $1 }


Rules  :                       { [] }
       | Rules Rule            { $2 : $1}
       | Rules Fact            { $2 : $1}

Rule: rule '<' VAR '>'  KVMap RuleVarDecls RulePrecond RuleConcl { Rule (tokenRange $1 $8) (tokenSym $3) $6 $7 $8 }
Fact: fact '<' VAR '>'  KVMap RuleVarDecls Expr { Rule (tokenRange $1 $7) (tokenSym $3) $6
                                                 (ValE (nullSRng) (BoolV True)) $7 }

RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }

KVMap :                        { [] }
| '{' KVMapListCommaSep  '}'   { reverse $2 }

KVMapListCommaSep :                      { [] }
            | KVPair                   { [$1] }
            | KVMapListCommaSep ',' KVPair  { $3 : $1 }

KVPair : VAR             { (tokenSym $1, MapVM []) }
       | VAR ':' VAR     { (tokenSym $1, IdVM $ tokenSym $3) }
       | VAR ':' true    { (tokenSym $1, BoolVM True) }
       | VAR ':' false   { (tokenSym $1, BoolVM False) }
       | VAR ':' NUM     { (tokenSym $1, IntVM $3) }
       | VAR ':' KVMap   { (tokenSym $1, MapVM $3) }

{

tokenSym    (L _ (TokenSym sym)) = sym
tokenString (L _ (TokenString str)) = str
tokenStringLit (L _ (TokenStringLit str)) = str

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
parseError :: Token -> Alex a
parseError (L p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

-- parseError :: [Token] -> Except String a
-- parseError (l:ls) = throwError (show l)
-- parseError [] = throwError "Unexpected end of Input"

parseProgram :: FilePath -> String -> Either Err (Program SRng)
parseProgram = runAlex' program

-- parseProgram:: String -> Either String (Program (Maybe ClassName) ())
-- parseProgram input = runExcept $ do
--   tokenStream <- scanTokens input
--   program tokenStream

-- still needed ???
-- parseTokens :: String -> Either String [Token]
-- parseTokens = runExcept . scanTokens

}
