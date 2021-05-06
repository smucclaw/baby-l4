{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore -}

module Parser (
  parseProgram
--  , parseTokens,
) where

import Lexer
import Annotation
import Syntax
import Parser.SmartConstructors

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
                                  --  { Program (tokenRangeList [getLoc $1, getLoc $2, getLoc $3, getLoc $4, getLoc $5]) (reverse $ unLoc $1) (reverse $2)  (reverse $3) (reverse $4) (reverse $5) }
                                   { loc2Ann $ Program mempty <&> fmap reverse $1 <*> lrev $2 <*> lrev $3 <*> lrev $4 <*> lrev $5 }

Lexicon :                   { L (DummySRng "No lexicon") [] }
        |  lexicon Mappings { $1 *> mkLocated $2 }

Mappings :                   {[]}
          | Mappings Mapping {$2 : $1 }
Mapping : VAR '->' STRLIT { loc2Ann $ Mapping mempty <&> (fmap tokenSym' $1) <*> (fmap tokenStringLit' $3) }

ClassDecls :                       { [] }
           | ClassDecls ClassDecl  { $2 : $1 }

ClassDecl : class VAR ClassDef     { -- special treatment: create Object class without superclass
                                     let defClass = if tokenSym $2 == "Object" then L mempty (ClassDef [] []) else $3
                                     in loc2Ann $ ClassDecl mempty <$ $1 <*> fmap (ClsNm . tokenSym') $2 <*> defClass
                                   }

ClassDef :   Fields                { ClassDef [ClsNm "Class"] <&> fmap reverse $1 }
         |   extends VAR Fields    { ClassDef <$ $1 <*> fmap ((:[]) . ClsNm . tokenSym') $2 <*> fmap reverse $3 }

Fields  :                          { mkLocated [] }
        | '{' FieldDecls '}'       { L (tokenRange $1 $3) $2 }

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
Assertion : assert Expr            { Assertion (tokenRange $1 $2) $2 }

-- Atomic type
-- Used to resolve ambigouity of     \x : A -> B -> x
-- and force the use of parenthesis: \x : (A -> B) -> x
ATp  : Bool                       { BoolT <$ $1 }
     | Int                        { IntT <$ $1 }
     | VAR                        { ClassT . ClsNm . tokenSym' <&> $1 }
    --  | VAR                        { L (getLoc $1) $ ClassT (ClsNm $ tokenSym $1) }
     | '(' TpsCommaSep ')'        { L (getLoc $2) $ case $2 of [t] -> unLoc t; tcs -> TupleT (map unLoc $ reverse tcs) }

TpsCommaSep :                      { [] }
            | Tp                   { [$1] }
            | TpsCommaSep ',' Tp   { $3 : $1 }

Tp   : ATp                        { $1 }
 --  | Tp '->' Tp                 { L (tokenRange $1 $3) $ FunT (unLoc $1) (unLoc $3) }
     | Tp '->' Tp                 { FunT <&> $1 <*> $3 }


Pattern : VAR                      { VarP $ tokenSym $1 }
    | '(' VarsCommaSep ')'         { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

VarsCommaSep :                      { [] }
            | VAR                   { [tokenSym $1] }
            | VarsCommaSep ',' VAR  { tokenSym $3 : $1 }

Expr : '\\' Pattern ':' ATp '->' Expr  { funE $1              $2 $4 $6 }
     | forall VAR ':' Tp '.' Expr      { quantifE All $1      $2 $4 $6 }
     | exists VAR ':' Tp '.' Expr      { quantifE Ex  $1      $2 $4 $6 }
     | Expr '-->' Expr                 { binOpE (BBool BBimpl)   $1 $3 }
     | Expr '||' Expr                  { binOpE (BBool BBor)     $1 $3 }
     | Expr '&&' Expr                  { binOpE (BBool BBand)    $1 $3 }
     | if Expr then Expr else Expr     { ifThenElseE $1       $2 $4 $6 }
     | not Expr                        { unaOpE (UBool UBneg)    $1 $2 }
     | not derivable VAR               { notDeriv True  $1 (varE $3)   }
     | not derivable not VAR           { notDeriv False $1 (varE $4)   }
     | not derivable VAR Atom          { notDeriv True  $1 (appE (varE $3) $4) }
     | not derivable not VAR Atom      { notDeriv False $1 (appE (varE $4) $5) }
     | Expr '<' Expr                   { binOpE (BCompar BClt)   $1 $3 }
     | Expr '<=' Expr                  { binOpE (BCompar BClte)  $1 $3 }
     | Expr '>' Expr                   { binOpE (BCompar BCgt)   $1 $3 }
     | Expr '>=' Expr                  { binOpE (BCompar BCgte)  $1 $3 }
     | Expr '=' Expr                   { binOpE (BCompar BCeq)   $1 $3 }
     | Expr '+' Expr                   { binOpE (BArith BAadd)   $1 $3 }
     | Expr '-' Expr                   { binOpE (BArith BAsub)   $1 $3 }
     | '-' Expr %prec AMINUS           { unaOpE (UArith UAminus) $1 $2 }
     | Expr '*' Expr                   { binOpE (BArith BAmul)   $1 $3 }
     | Expr '/' Expr                   { binOpE (BArith BAdiv)   $1 $3 }
     | Expr '%' Expr                   { binOpE (BArith BAmod)   $1 $3 }
     | App                             { $1 }

App : App Acc                      { appE $1 $2 }
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
Rule: rule '<' VAR '>'  RuleVarDecls RulePrecond RuleConcl { Rule (tokenRange $1 $7) (tokenSym $3) $5 $6 $7 }

-- RuleName: rule '<' VAR '>'     { tokenSym $3 }

RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }

{


-- Alias for <$> because the happy parser is being silly
(<&>) :: Functor f => (a -> b) -> f a -> f b
(<&>) = fmap
infixl 4 <&>

lrev :: HasLoc a => [a] -> Located [a]
lrev = mkLocated . reverse

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
