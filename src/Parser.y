{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore -}

{-

    Checklist what to do when modifying the language:
    * In Lexer.x (when introducing new lexical items):
    - Below "tokens :-", add the textual representations and tokens
    - Below "data TokenKind", add the tokens
    - Extend the "unlex" function definition
    * in Parser.y:
    - Below "%tokens", add the tokens
    - Below "Operators", possibly specify associativity / priorities of operators
    - Modify the grammar
-}

module Parser (
  parseNewProgram,
  parseProgram
--  , parseTokens,
) where

import Lexer
import Annotation
import KeyValueMap
import Syntax

import Prelude
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Either.Combinators (mapRight)

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
    extends { L _ TokenExtends }
    lexicon { L _ TokenLexicon }
    fact    { L _ TokenFact }
    rule    { L _ TokenRule }
    derivable { L _ TokenDerivable }

    process     { L _ TokenProcess }
    clock       { L _ TokenClock }
    state       { L _ TokenState }
    init        { L _ TokenInit }
    trans       { L _ TokenTrans }
    guard       { L _ TokenGuard }
    assign      { L _ TokenAssign }

    not    { L _ TokenNot }
    forall { L _ TokenForall }
    exists { L _ TokenExists }
    if     { L _ TokenIf }
    then   { L _ TokenThen }
    else   { L _ TokenElse }
    for    { L _ TokenFor }
    true   { L _ TokenTrue }
    false  { L _ TokenFalse }

    'A<>'  { L _ TokenAF }
    'A[]'  { L _ TokenAG }
    'E<>'  { L _ TokenEF }
    'E[]'  { L _ TokenEG }

    '\\'  { L _ TokenLambda }
    '->'  { L _ TokenArrow }
    '-->' { L _ TokenImpl }
    '||'  { L _ TokenOr }
    '&&'  { L _ TokenAnd }
    '=='  { L _ TokenEq }
    '<'   { L _ TokenLt }
    '<='  { L _ TokenLte }
    '>'   { L _ TokenGt }
    '>='  { L _ TokenGte }
    '/='  { L _ TokenNe }
    '='   { L _ TokenAssignTo }
    '+'   { L _ TokenAdd }
    '-'   { L _ TokenSub }
    '*'   { L _ TokenMul }
    '/'   { L _ TokenDiv }
    '%'   { L _ TokenMod }
    '.'   { L _ TokenDot }
    ','   { L _ TokenComma }
    ':'   { L _ TokenColon }
    ';'   { L _ TokenSemicolon }
    '('   { L _ TokenLParen }
    ')'   { L _ TokenRParen }
    '{'   { L _ TokenLBrace }
    '}'   { L _ TokenRBrace }

    INT   { L pos (TokenInteger $$) }
    FLT   { L pos (TokenFloat $$) }
    VAR   { L _ (TokenSym _) }
    STRLIT { L _ (TokenStringLit _)}
    STR   { L _ (TokenString _) }

-- Operators
%right '->'
%nonassoc '.'                          -- required for quantifier rules
%nonassoc 'A<>' 'A[]' 'E<>' 'E[]'
%nonassoc if then else
%right '-->'
%right '||'
%right '&&'
%left not
%nonassoc '<' '<=' '==' '/=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left AMINUS
%%

QualifVar : VAR { QVarName (getLoc $1) (tokenSym $1) }

--Program : Lexicon ClassDecls GlobalVarDecls Rules Assertions
--                                   { Program (tokenRangeList [getLoc $1, getLoc $2, getLoc $3, getLoc $4, getLoc $5]) (reverse $ unLoc $1) (reverse $2)  (reverse $3) (reverse $4) (reverse $5) }

Program : TopLevelElements { NewProgram (tokenRangeList (map getLoc $1)) (reverse $1) }

TopLevelElements : TopLevelElement                  { [$1] }
                 | TopLevelElementGroup             { $1 }
                 | TopLevelElements TopLevelElement { $2 : $1 }

-- TopLevelElementGroup: list of top level elements grouped together below one keyword
TopLevelElementGroup : Mappings { map MappingTLE $1 } 

TopLevelElement : ClassDecl     { ClassDeclTLE $1 } 
                | GlobalVarDecl { VarDeclTLE $1 } 
                | RuleOrFact    { RuleTLE $1 }
                | Assertion     { AssertionTLE $1 }
                | Automaton     { AutomatonTLE $1 }

-- { L (DummySRng "No lexicon") [] }
--Lexicon :  lexicon Mapping  { L (tokenRange $1 $2) [$2] }
--|  lexicon Mapping Mappings { L (coordFromTo (getLoc $1) (tokenRangeList $3)) $2 }
--        |  lexicon Mapping Mappings { L (tokenRangeList [getLoc $1, getLoc $2]) $2 }

-- Lexicon : lexicon '{' Mappings '}' { L (tokenRange $1 $4) $3 }

Mappings :  LexiconMapping  { [$1] }
          | Mappings Mapping { $2 : $1 }

LexiconMapping : lexicon VAR '->' STRLIT { Mapping (tokenRange $1 $4) (tokenSym $2) (tokenStringLit $4) }
Mapping        : VAR '->' STRLIT { Mapping (tokenRange $1 $3) (tokenSym $1) (tokenStringLit $3) }

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

FieldDecl : VAR ':' Tp             { FieldDecl (tokenRange $1 $3) (FldNm $ tokenSym $1) $3 }

GlobalVarDecls :                         { [] }
         | GlobalVarDecls GlobalVarDecl  { $2 : $1 }

GlobalVarDecl : decl VAR ':' Tp          { VarDecl (tokenRange $1 $4) (tokenSym $2) $4 }

VarDeclsCommaSep :  VarDecl              { [$1] }
         | VarDeclsCommaSep  ',' VarDecl { $3 : $1 }

VarDecl : VAR ':' Tp                     { VarDecl (tokenRange $1 $3) (tokenSym $1) $3 }


-- Atomic type
-- Used to resolve ambigouity of     \x : A -> B -> x
-- and force the use of parenthesis: \x : (A -> B) -> x
ATp  : VAR                        { ClassT (getLoc $1) (ClsNm $ tokenSym $1) }
| '(' TpsCommaSep ')'        { case $2 of [t] -> t{annotOfTp = (tokenRange $1 $3)}; tcs -> TupleT (tokenRange $1 $3) (reverse tcs) }

TpsCommaSep :                      { [] }
            | Tp                   { [$1] }
            | TpsCommaSep ',' Tp   { $3 : $1 }

Tp   : ATp                        { $1 }
     | Tp '->' Tp                 { FunT (tokenRange $1 $3) $1 $3 }


Pattern : QualifVar                { VarP $1 }
    | '(' QVarsCommaSep ')'        { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

QVarsCommaSep :                            { [] }
            | QualifVar                    { [$1] }
            | QVarsCommaSep ',' QualifVar  { $3 : $1 }

VarsCommaSep :                            { [] }
            | VAR                    { [tokenSym $1] }
            | VarsCommaSep ',' VAR  { (tokenSym $3) : $1 }

Expr : '\\' Pattern ':' ATp '->' Expr  { FunE (tokenRange $1 $6) $2 $4 $6 }
     | forall QualifVar ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) All $2 $4 $6 }
     | exists QualifVar ':' Tp '.' Expr      { QuantifE (tokenRange $1 $6) Ex  $2 $4 $6 }
     | 'A<>' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTAF) $2 }
     | 'A[]' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTAG) $2 }
     | 'E<>' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTEF) $2 }
     | 'E[]' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTEG) $2 }
     | Expr '-->' Expr             { BinOpE (tokenRange $1 $3) (BBool BBimpl) $1 $3 }
     | Expr '||' Expr              { BinOpE (tokenRange $1 $3) (BBool BBor) $1 $3 }
     | Expr '&&' Expr              { BinOpE (tokenRange $1 $3) (BBool BBand) $1 $3 }
     | if Expr then Expr else Expr { IfThenElseE (tokenRange $1 $6) $2 $4 $6 }
     | not Expr                    { UnaOpE (tokenRange $1 $2) (UBool UBnot) $2 }
     | not derivable QualifVar           { NotDeriv (tokenRange $1 $3) True (VarE (getLoc $3) (GlobalVar $3)) }
     | not derivable not QualifVar       { NotDeriv (tokenRange $1 $4) False (VarE (getLoc $4) (GlobalVar $4)) }
     | not derivable QualifVar Atom      { NotDeriv (tokenRange $1 $4) True (AppE (tokenRange $3 $4)  (VarE (getLoc $3) (GlobalVar $3)) $4) }
     | not derivable not QualifVar Atom  { NotDeriv (tokenRange $1 $5) False (AppE (tokenRange $4 $5) (VarE (getLoc $4) (GlobalVar $4)) $5) }
     | Expr '<' Expr               { BinOpE (tokenRange $1 $3) (BCompar BClt) $1 $3 }
     | Expr '<=' Expr              { BinOpE (tokenRange $1 $3) (BCompar BClte) $1 $3 }
     | Expr '>' Expr               { BinOpE (tokenRange $1 $3) (BCompar BCgt) $1 $3 }
     | Expr '>=' Expr              { BinOpE (tokenRange $1 $3) (BCompar BCgte) $1 $3 }
     | Expr '==' Expr              { BinOpE (tokenRange $1 $3) (BCompar BCeq) $1 $3 }
     | Expr '/=' Expr              { BinOpE (tokenRange $1 $3) (BCompar BCne) $1 $3 }
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
     | INT                         { ValE (pos) (IntV $1) }
     | FLT                         { ValE (pos) (FloatV $1) }
     | STR                         { ValE (tokenPos $1) (StringV (tokenString $1)) }
     | QualifVar                   { VarE (getLoc $1) (GlobalVar $1) }
     | true                        { ValE (tokenPos $1) (BoolV True) }
     | false                       { ValE (tokenPos $1) (BoolV False) }

ExprsCommaSep :                      { [] }
            | Expr                   { [$1] }
            | ExprsCommaSep ',' Expr  { $3 : $1 }


----------------------------------------------------------------------
-- Rules and Assertions
----------------------------------------------------------------------

-- Assertion / Rule name
ARName :                 { Nothing }
       | '<' VAR '>'     { Just (tokenSym $2) }

Rules  :                       { [] }
       | Rules Rule            { $2 : $1}
       | Rules Fact            { $2 : $1}

RuleOrFact : Rule { $1 }
           | Fact { $1 }
-- TODO: KVMaps do not have a location, so the token range in the following is incomplete
Rule : rule ARName KVMap { Rule (getLoc $1) $2 $3  [] (ValE (nullSRng) (BoolV True)) (ValE (nullSRng) (BoolV True)) }
     | rule ARName KVMap RuleVarDecls RulePrecond RuleConcl { Rule (tokenRange $1 $6) $2 $3 $4 $5 $6 }

Fact : fact ARName  KVMap RuleVarDecls Expr { Rule (tokenRange $1 $5) $2 $3 $4 (ValE (nullSRng) (BoolV True)) $5 }



-- TODO: labellings still to be added, channels to be added
-- TODO: annotation is a rough approximation, to be synthesized from annotations of subexpressions
Automaton : Clocks 
  process VAR '(' ')' '{' States Initial Transitions '}'
  { TA {annotOfTA = (tokenRange $2 $5),
        nameOfTA = (tokenSym $3), locsOfTA = (map fst $7), channelsOfTA = [], clocksOfTA = $1,
        transitionsOfTA = $9, initialLocsOfTA = $8, invarsOfTA = $7, labellingOfTA = []}}


-- Channels : channels VARsCommaSep ';' { map ClsNm $2 }

Clocks : clock VarsCommaSep ';' { map Clock $2 }

States : state StatesCommaSep ';' { $2 }

StatesCommaSep :                            { [] }
            | StateWithInvar                    { [$1] }
            | StatesCommaSep ',' StateWithInvar  { $3 : $1 }

StateWithInvar : VAR { (Loc (tokenSym $1), []) }
               | VAR '{' InvarsAndSep '}' { (Loc (tokenSym $1), $3) }
		 
InvarsAndSep :                        { [] }
            | Invar                     { [$1] }
            | InvarsAndSep '&&' Invar  { $3 : $1 }

-- TODO: refine the notion of invariant
Invar : VAR '<' INT  { ClConstr (Clock (tokenSym $1)) BClt $3 }
      | VAR '<=' INT { ClConstr (Clock (tokenSym $1)) BClte $3 }
      | VAR '>' INT  { ClConstr (Clock (tokenSym $1)) BCgt $3 }
      | VAR '>=' INT { ClConstr (Clock (tokenSym $1)) BCgte $3 }
      | VAR '==' INT { ClConstr (Clock (tokenSym $1)) BCeq $3 }
      | VAR '/=' INT { ClConstr (Clock (tokenSym $1)) BCne $3 }

-- Here just one initial variable
Initial : init VAR  ';' { [Loc (tokenSym $2)] }

Transitions : trans TransitionsCommaSep ';' { $2 }

TransitionsCommaSep :                            { [] }
            | TransitionWithInfo                 { [$1] }
            | TransitionsCommaSep ',' TransitionWithInfo  { $3 : $1 }

TransitionWithInfo : VAR '->' VAR '{' TrGuard TrAssign '}'
                 { Transition {sourceOfTransition = (Loc (tokenSym $1)),
		   	       guardOfTransition = $5,
			       actionOfTransition = $6,
			       targetOfTransition = (Loc (tokenSym $3))} }

-- TODO: so far, guard expressions are not taken into account, only clock constraints
TrGuard :                                   { TransitionGuard [] (ValE (nullSRng) (BoolV True)) }
| guard InvarsAndSep ';' { TransitionGuard $2 (ValE (nullSRng) (BoolV True)) }

-- TODO: only clock resets taken into account
TrAssign :                                   { TransitionAction Internal [] (Skip (nullSRng)) }
	 | assign TrAssignmentsCommaSep ';'  { TransitionAction Internal $2 (Skip (nullSRng)) }


TrAssignmentsCommaSep :                             { [] }
            | TrAssignment                          { [$1] }
            | TrAssignmentsCommaSep ',' TrAssignment  { $3 : $1 }

-- TODO: assignments in Uppaal can also be to other variables and other than clock resets
-- The value assigned is here not taken into account
TrAssignment : VAR '=' INT { Clock (tokenSym $1)  }


RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }


Assertions :                       { [] }
           | Assertions Assertion  { $2 : $1 }

-- TODO: same problem with locations as for Rule above
Assertion : assert ARName KVMap        { Assertion (getLoc $1) $2 $3 (ValE (nullSRng) (BoolV True)) }
          | assert ARName KVMap Expr   { Assertion (tokenRange $1 $4) $2 $3 $4 }


KVMap :                        { [] }
| '{' KVMapListCommaSep  '}'   { reverse $2 }

KVMapListCommaSep :                      { [] }
            | KVPair                   { [$1] }
            | KVMapListCommaSep ',' KVPair  { $3 : $1 }

KVPair : VAR             { (tokenSym $1, MapVM []) }
       | VAR ':' VAR     { (tokenSym $1, IdVM $ tokenSym $3) }
       | VAR ':' true    { (tokenSym $1, BoolVM True) }
       | VAR ':' false   { (tokenSym $1, BoolVM False) }
       | VAR ':' INT     { (tokenSym $1, IntVM $3) }
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

parseNewProgram :: FilePath -> String -> Either Err (NewProgram SRng)
parseNewProgram = runAlex' program

parseProgram :: FilePath -> String -> Either Err (Program SRng)
parseProgram fp inp = mapRight newProgramToProgram (parseNewProgram fp inp)
}
