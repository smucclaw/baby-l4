{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- HLINT ignore -}

{-

    Checklist what to do when modifying the language:
    * In L4.Lexer.x (when introducing new lexical items):
    - Below "tokens :-", add the textual representations and tokens
    - Below "data TokenKind", add the tokens
    - Extend the "unlex" function definition
    * in Parser.y:
    - Below "%token", add the tokens
    - Below "Operators", possibly specify associativity / priorities of operators
    - Modify the grammar
-}

module L4.Parser (
  parseProgram,
--  parseProgram
--  , parseTokens,
) where

import L4.Lexer
import L4.Annotation
import L4.KeyValueMap
import L4.Syntax

import Prelude
import Control.Monad.Except
import Data.Maybe (fromMaybe)

}

-- Entry point
%name program Program

-- L4.Lexer structure
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

    system      { L _ TokenSystem }
    process     { L _ TokenProcess }
    chan        { L _ TokenChannel }
    clock       { L _ TokenClock }
    state       { L _ TokenState }
    urgent      { L _ TokenUrgent }
    init        { L _ TokenInit }
    trans       { L _ TokenTrans }
    guard       { L _ TokenGuard }
    sync        { L _ TokenSync }
    assign      { L _ TokenAssign }
    uppaalBool  { L _ TokenUppaalBool }
    uppaalInt   { L _ TokenUppaalInt }

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
    '?'   { L _ TokenQuestMark }
    '!'   { L _ TokenExclMark }
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


----------------------------------------------------------------------
-- Programs and top level elements
----------------------------------------------------------------------

Program : TopLevelElements { Program (tokenRangeList (map getLoc $1)) (reverse $1) }

TopLevelElements : TopLevelElement                  { [$1] }
                 | TopLevelElementGroup             { $1 }
                 | TopLevelElements TopLevelElement { $2 : $1 }

-- TopLevelElementGroup: list of top level elements grouped together below one keyword
TopLevelElementGroup : Mappings { map MappingTLE $1 } 

TopLevelElement : ClassDecl     { ClassDeclTLE $1 } 
                | GlobalVarDecl { VarDeclTLE $1 } 
                | GlobalVarDefn { VarDefnTLE $1 } 
                | RuleOrFact    { RuleTLE $1 }
                | Assertion     { AssertionTLE $1 }
                | Automaton     { AutomatonTLE $1 }
                | System        { SystemTLE $1 }

-- { L (DummySRng "No lexicon") [] }
--Lexicon :  lexicon Mapping  { L (tokenRange $1 $2) [$2] }
--|  lexicon Mapping Mappings { L (coordFromTo (getLoc $1) (tokenRangeList $3)) $2 }
--        |  lexicon Mapping Mappings { L (tokenRangeList [getLoc $1, getLoc $2]) $2 }

-- Lexicon : lexicon '{' Mappings '}' { L (tokenRange $1 $4) $3 }

Mappings :  LexiconMapping  { [$1] }
          | Mappings Mapping { $2 : $1 }

LexiconMapping : lexicon VAR '->' STRLIT { Mapping (tokenRange $1 $4) (tokenSym $2) (parseDescription $ tokenStringLit $4) }
Mapping        : VAR '->' STRLIT { Mapping (tokenRange $1 $3) (tokenSym $1) (parseDescription $ tokenStringLit $3) }

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

GlobalVarDecl : decl VAR ':' Tp          { VarDecl (tokenRange $1 $4) (tokenSym $2) $4 }

GlobalVarDefn : defn VAR ':' Tp '=' Expr     
   { VarDefn (tokenRange $1 $6) (tokenSym $2) $4 $6 }

VarDeclsCommaSep :  VarDecl              { [$1] }
         | VarDeclsCommaSep  ',' VarDecl { $3 : $1 }

VarDecl    : VAR ':' Tp                     { VarDecl (tokenRange $1 $3) (tokenSym $1) $3 }
VarDeclATp : VAR ':' ATp                    { VarDecl (tokenRange $1 $3) (tokenSym $1) $3 }

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

QualifVar : VAR { QVarName (getLoc $1) (tokenSym $1) }

-- Patterns currently not used because predicates are typically written in a curried style
Pattern : QualifVar                { VarP $1 }
    | '(' QVarsCommaSep ')'        { let vcs = $2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs) }

-- qualified variable list in reverse order
QVarsCommaSep :                            { [] }
            | QualifVar                    { [$1] }
            | QVarsCommaSep ',' QualifVar  { $3 : $1 }

-- variable list in reverse order
VarsCommaSep :                       { [] }
            | VAR                    { [tokenSym $1] }
            | VarsCommaSep ',' VAR   { (tokenSym $3) : $1 }


----------------------------------------------------------------------
-- Expressions
----------------------------------------------------------------------

Expr : '\\' VarDeclATp '->' Expr    { FunE (tokenRange $1 $4) $2 $4 }
     | forall VarDecl '.' Expr      { QuantifE (tokenRange $1 $4) All $2 $4 }
     | exists VarDecl '.' Expr      { QuantifE (tokenRange $1 $4) Ex  $2 $4 }
     | 'A<>' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTAF) $2 }
     | 'A[]' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTAG) $2 }
     | 'E<>' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTEF) $2 }
     | 'E[]' Expr                  { UnaOpE (tokenRange $1 $2) (UTemporal UTEG) $2 }
     | Expr '-->' Expr             { BinOpE (tokenRange $1 $3) (BBool BBimpl) $1 $3 }
     | Expr '||' Expr              { BinOpE (tokenRange $1 $3) (BBool BBor) $1 $3 }
     | Expr '&&' Expr              { BinOpE (tokenRange $1 $3) (BBool BBand) $1 $3 }
     | if Expr then Expr else Expr { IfThenElseE (tokenRange $1 $6) $2 $4 $6 }
     | not Expr                    { UnaOpE (tokenRange $1 $2) (UBool UBnot) $2 }
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

RuleVarDecls :                       { [] }
             | for VarDeclsCommaSep  { reverse $2 }

RulePrecond : if Expr      { $2 }
RuleConcl   : then Expr    { $2 }

Assertions :                       { [] }
           | Assertions Assertion  { $2 : $1 }

-- TODO: same problem with locations as for Rule above
Assertion : assert ARName KVMap        { Assertion (getLoc $1) $2 $3 (ValE (nullSRng) (BoolV True)) }
          | assert ARName KVMap Expr   { Assertion (tokenRange $1 $4) $2 $3 $4 }


----------------------------------------------------------------------
-- Automata and Systems of automata
----------------------------------------------------------------------
System : system VAR '{' SystemVarDecls Channels AutomatonList '}'
  { TASys {annotOfSys = (tokenRange $1 $7),
           nameOfTASys = (tokenSym $2),
           declsOfSys = reverse $4,
           channelsOfSys = $5,
           automataOfSys = reverse $6
           } }

-- system var decls in reverse order
SystemVarDecls :                         { [] }
         | SystemVarDecls GlobalVarDecl  { $2 : $1 }
         | SystemVarDecls uppaalBoolDecls  { $2 ++ $1 }
         | SystemVarDecls uppaalIntDecls  { $2 ++ $1 }

-- Uppaal bool variable decls in reverse order
uppaalBoolDecls : uppaalBool QVarsCommaSep ';' 
  { map (\(QVarName ann v) -> VarDecl ann v (ClassT (getLoc $1) BooleanC)) $2 }

-- Uppaal int variable decls in reverse order
uppaalIntDecls : uppaalInt QVarsCommaSep ';' 
  { map (\(QVarName ann v) -> VarDecl ann v (ClassT (getLoc $1) IntegerC)) $2 }

-- TODO: annotation is a rough approximation, to be synthesized from annotations of subexpressions
-- NOTE: The Labellings component is specific to L4 and is not part of the Uppaal syntax.
--       In Uppaal, labellings can somehow be coded in invariants, see note on invariants below
Automaton : process VAR '(' ')' '{' Clocks States Urgents Initial Transitions Labellings '}'
  { TA {annotOfTA = (tokenRange $1 $12),
        nameOfTA = (tokenSym $2), locsOfTA = (map fst $7), clocksOfTA = $6,
        transitionsOfTA = $10, urgentLocsOfTA = $8,
        initialLocOfTA = $9, invarsOfTA = $7, labellingOfTA = $11}}

-- automaton list in reverse order
AutomatonList :                          { [] }
              | AutomatonList Automaton  { $2 : $1 }

-- channel list in normal order
Channels :                           { [] }
         | chan VarsCommaSep ';' { reverse $2 }

-- clock list in normal order
Clocks :                        { [] }
       | clock VarsCommaSep ';' { map Clock (reverse $2) }

-- state (with invariant) list in normal order
States : state StatesCommaSep ';' { reverse $2 }

-- urgent states (without invariant) in normal order
Urgents :                         { [] }
        | urgent VarsCommaSep ';' { map Loc (reverse $2) } 

-- state (with invariant) list in reverse order
StatesCommaSep :                            { [] }
            | StateWithInvar                    { [$1] }
            | StatesCommaSep ',' StateWithInvar  { $3 : $1 }

StateWithInvar : VAR { (Loc (tokenSym $1), []) }
               | VAR '{' InvarsAndSep '}' { (Loc (tokenSym $1), reverse $3) }
		 
-- invariant list in reverse order
InvarsAndSep :                        { [] }
            | Invar                     { [$1] }
            | InvarsAndSep '&&' Invar  { $3 : $1 }

-- TODO: refine the notion of invariant.
-- NOTE: In Uppaal, invariants can be boolean combinations of clock constraints (as below) and 
--       other expressions. The precise syntactic conditions are not clear.
Invar : VAR '<' INT  { ClConstr (Clock (tokenSym $1)) BClt $3 }
      | VAR '<=' INT { ClConstr (Clock (tokenSym $1)) BClte $3 }
      | VAR '>' INT  { ClConstr (Clock (tokenSym $1)) BCgt $3 }
      | VAR '>=' INT { ClConstr (Clock (tokenSym $1)) BCgte $3 }
      | VAR '==' INT { ClConstr (Clock (tokenSym $1)) BCeq $3 }
      | VAR '/=' INT { ClConstr (Clock (tokenSym $1)) BCne $3 }

Initial : init VAR  ';' { Loc (tokenSym $2) }

-- transistions list in normal order
Transitions : trans TransitionsCommaSep ';' { reverse $2 }

-- transistions list in reverse order
TransitionsCommaSep :                            { [] }
            | TransitionWithInfo                 { [$1] }
            | TransitionsCommaSep ',' TransitionWithInfo  { $3 : $1 }

TransitionWithInfo : VAR '->' VAR '{' TrGuard TrSync TrAssign '}'
           { Transition {sourceOfTransition = (Loc (tokenSym $1)),
		   	     guardOfTransition = $5,
             syncOfTransition = $6,
			       actionOfTransition = $7,
			       targetOfTransition = (Loc (tokenSym $3))} }

-- TODO: so far, guard expressions are not taken into account, only clock constraints
TrGuard :                                   { TransitionGuard [] Nothing }
| guard InvarsAndSep ';' { TransitionGuard (reverse $2) Nothing }

-- Note: the Broadcast mode is not a valid mode in Upppaal
SyncMode :  {Broadcast} | '!' {Send} | '?' {Receive}
TrSync :                    { Nothing }
	 | sync VAR SyncMode ';'  { Just (Sync (tokenSym $2) $3) }

-- TODO: only clock resets taken into account
TrAssign :                             { TransitionAction [] (Skip (nullSRng)) }
	 | assign TrAssignmentsCommaSep ';'  { TransitionAction (reverse $2) (Skip (nullSRng)) }

-- transition assignment list in reverse order
TrAssignmentsCommaSep :                             { [] }
            | TrAssignment                          { [$1] }
            | TrAssignmentsCommaSep ',' TrAssignment  { $3 : $1 }

-- TODO: assignments in Uppaal can also be to other variables and other than clock resets
-- The value assigned is here not taken into account
TrAssignment : VAR '=' INT { Clock (tokenSym $1)  }

-- TODO: to be defined
Labellings : VAR   {[]}

----------------------------------------------------------------------
-- Key-Value-Maps (used as instructions in rules and assertions)
----------------------------------------------------------------------

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

parseProgram :: FilePath -> String -> Either Err (Program SRng)
parseProgram = runAlex' program

}
