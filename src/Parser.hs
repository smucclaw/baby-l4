{-# OPTIONS_GHC -w #-}
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
--  parseProgram
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,528) ([0,0,0,57344,30,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,52736,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,2,0,0,0,0,0,0,256,0,0,0,0,0,1024,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,4,0,0,0,0,0,0,32,0,0,0,0,0,64,0,0,0,0,0,0,0,32768,0,0,0,0,2048,0,0,1024,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,16271,1024,1476,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,4,0,0,0,0,0,0,1024,0,0,0,0,16384,0,0,0,0,0,0,0,0,16,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,256,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,16271,1024,1476,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,16384,16,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,4,0,0,0,0,0,0,32,0,0,0,0,0,0,8320,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,61432,3,0,0,0,0,0,12,8192,46,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,16271,1024,1476,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,64,0,0,0,0,61920,32775,47232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15360,254,4112,23,0,0,0,0,32542,2048,2952,0,0,0,0,36608,63,50180,5,0,0,0,32768,8135,512,738,0,0,0,0,0,0,16384,0,0,0,0,57344,2033,32896,184,0,0,0,0,63728,16387,23616,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32704,31,0,0,0,0,0,0,8192,2,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,8,0,0,0,0,0,65024,251,0,0,0,0,0,0,32255,0,0,0,0,0,0,65408,62,0,0,0,0,0,49152,8063,0,0,0,0,0,1024,49120,15,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,16,0,0,0,0,0,0,512,0,0,0,0,0,0,64496,0,0,0,0,0,0,0,0,2,0,0,0,0,0,16384,0,0,0,0,0,51072,31,57858,2,0,0,0,49152,4067,256,369,0,0,0,0,61920,32775,47232,0,0,0,0,61440,1016,16448,92,0,0,0,0,64632,8193,11808,0,0,0,0,15360,254,4112,23,0,0,0,0,32542,2048,2952,0,0,0,0,36608,63,50180,5,0,0,0,32768,8135,512,738,0,0,0,0,58304,15,28929,1,0,0,0,57344,2033,32896,184,0,0,0,0,63728,16387,23616,0,0,0,0,30720,508,8224,46,0,0,0,0,65084,4096,5904,0,0,0,0,0,3,40960,2,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,8704,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,1024,1,0,0,0,0,49152,8063,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,30720,508,8224,46,0,0,0,4096,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,32704,31,0,0,0,0,0,0,0,0,0,0,0,0,61920,32775,47232,0,0,0,0,0,0,0,16,0,0,0,0,0,2,0,0,0,0,0,0,0,4096,4,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3584,0,0,0,0,0,0,0,7,0,0,0,0,0,0,992,0,0,0,0,0,0,61440,1,0,0,0,0,0,0,248,0,0,0,0,0,0,31744,0,0,0,0,0,0,0,62,0,0,0,0,0,0,7936,0,0,0,0,0,0,49024,15,0,0,0,0,0,57344,2015,0,0,0,0,0,0,61432,3,0,0,0,0,0,0,0,0,0,0,0,0,65084,4096,5904,0,0,0,0,0,0,2048,2,0,0,0,0,16271,1024,1476,0,0,0,0,51072,31,57858,2,0,0,0,0,0,0,65,0,0,0,0,61920,32775,47232,0,0,0,0,61440,1016,16448,92,0,0,0,0,0,0,0,0,0,0,0,0,65024,251,0,0,0,0,0,0,32255,0,0,0,0,0,0,0,0,0,0,0,0,0,49168,8063,0,0,0,0,0,0,49120,15,0,0,0,0,0,2048,0,0,0,0,0,0,0,61432,3,0,0,0,0,0,512,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,16127,0,0,0,0,0,0,0,320,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,32,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,63728,16387,23616,0,0,0,0,0,64512,503,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,32,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,64512,0,0,0,0,0,0,0,16384,1,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,16,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,4,0,0,0,0,0,0,512,0,0,0,0,0,0,0,1,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,4096,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,16,16,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,512,0,0,0,0,0,0,640,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_program","QualifVar","Program","TopLevelElements","TopLevelElementGroup","TopLevelElement","Mappings","LexiconMapping","Mapping","ClassDecls","ClassDecl","ClassDef","Fields","FieldDecls","FieldDecl","GlobalVarDecls","GlobalVarDecl","VarDeclsCommaSep","VarDecl","VarDeclATp","ATp","TpsCommaSep","Tp","Pattern","QVarsCommaSep","VarsCommaSep","Expr","App","Acc","Atom","ExprsCommaSep","ARName","Rules","RuleOrFact","Rule","Fact","Automaton","Clocks","States","StatesCommaSep","StateWithInvar","InvarsAndSep","Invar","Initial","Transitions","TransitionsCommaSep","TransitionWithInfo","TrGuard","TrAssign","TrAssignmentsCommaSep","TrAssignment","RuleVarDecls","RulePrecond","RuleConcl","Assertions","Assertion","KVMap","KVMapListCommaSep","KVPair","assert","class","decl","extends","lexicon","fact","rule","process","clock","state","init","trans","guard","assign","not","forall","exists","if","then","else","for","true","false","'A<>'","'A[]'","'E<>'","'E[]'","'\\\\'","'->'","'-->'","'||'","'&&'","'=='","'<'","'<='","'>'","'>='","'/='","'='","'+'","'-'","'*'","'/'","'%'","'.'","','","':'","';'","'('","')'","'{'","'}'","INT","FLT","VAR","STRLIT","STR","%eof"]
        bit_start = st Prelude.* 119
        bit_end = (st Prelude.+ 1) Prelude.* 119
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..118]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (62) = happyShift action_16
action_0 (63) = happyShift action_17
action_0 (64) = happyShift action_18
action_0 (66) = happyShift action_19
action_0 (67) = happyShift action_20
action_0 (68) = happyShift action_21
action_0 (69) = happyShift action_22
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (13) = happyGoto action_9
action_0 (19) = happyGoto action_10
action_0 (36) = happyGoto action_11
action_0 (37) = happyGoto action_12
action_0 (38) = happyGoto action_13
action_0 (39) = happyGoto action_14
action_0 (58) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (116) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (119) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (62) = happyShift action_16
action_4 (63) = happyShift action_17
action_4 (64) = happyShift action_18
action_4 (67) = happyShift action_20
action_4 (68) = happyShift action_21
action_4 (69) = happyShift action_22
action_4 (8) = happyGoto action_33
action_4 (13) = happyGoto action_9
action_4 (19) = happyGoto action_10
action_4 (36) = happyGoto action_11
action_4 (37) = happyGoto action_12
action_4 (38) = happyGoto action_13
action_4 (39) = happyGoto action_14
action_4 (58) = happyGoto action_15
action_4 _ = happyReduce_2

action_5 _ = happyReduce_4

action_6 _ = happyReduce_3

action_7 (116) = happyShift action_32
action_7 (11) = happyGoto action_31
action_7 _ = happyReduce_6

action_8 _ = happyReduce_12

action_9 _ = happyReduce_7

action_10 _ = happyReduce_8

action_11 _ = happyReduce_9

action_12 _ = happyReduce_92

action_13 _ = happyReduce_93

action_14 _ = happyReduce_11

action_15 _ = happyReduce_10

action_16 (95) = happyShift action_25
action_16 (34) = happyGoto action_30
action_16 _ = happyReduce_87

action_17 (116) = happyShift action_29
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (116) = happyShift action_28
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (116) = happyShift action_27
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (95) = happyShift action_25
action_20 (34) = happyGoto action_26
action_20 _ = happyReduce_87

action_21 (95) = happyShift action_25
action_21 (34) = happyGoto action_24
action_21 _ = happyReduce_87

action_22 (116) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (110) = happyShift action_46
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (112) = happyShift action_36
action_24 (59) = happyGoto action_45
action_24 _ = happyReduce_136

action_25 (116) = happyShift action_44
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (112) = happyShift action_36
action_26 (59) = happyGoto action_43
action_26 _ = happyReduce_136

action_27 (90) = happyShift action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (108) = happyShift action_41
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (65) = happyShift action_39
action_29 (112) = happyShift action_40
action_29 (14) = happyGoto action_37
action_29 (15) = happyGoto action_38
action_29 _ = happyReduce_21

action_30 (112) = happyShift action_36
action_30 (59) = happyGoto action_35
action_30 _ = happyReduce_136

action_31 _ = happyReduce_13

action_32 (90) = happyShift action_34
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_5

action_34 (117) = happyShift action_83
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (76) = happyShift action_67
action_35 (77) = happyShift action_68
action_35 (78) = happyShift action_69
action_35 (79) = happyShift action_70
action_35 (83) = happyShift action_71
action_35 (84) = happyShift action_72
action_35 (85) = happyShift action_73
action_35 (86) = happyShift action_74
action_35 (87) = happyShift action_75
action_35 (88) = happyShift action_76
action_35 (89) = happyShift action_77
action_35 (102) = happyShift action_78
action_35 (110) = happyShift action_79
action_35 (114) = happyShift action_80
action_35 (115) = happyShift action_81
action_35 (116) = happyShift action_2
action_35 (118) = happyShift action_82
action_35 (4) = happyGoto action_62
action_35 (29) = happyGoto action_63
action_35 (30) = happyGoto action_64
action_35 (31) = happyGoto action_65
action_35 (32) = happyGoto action_66
action_35 _ = happyReduce_134

action_36 (116) = happyShift action_61
action_36 (60) = happyGoto action_59
action_36 (61) = happyGoto action_60
action_36 _ = happyReduce_138

action_37 _ = happyReduce_18

action_38 _ = happyReduce_19

action_39 (116) = happyShift action_58
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (16) = happyGoto action_57
action_40 _ = happyReduce_23

action_41 (110) = happyShift action_55
action_41 (116) = happyShift action_56
action_41 (23) = happyGoto action_53
action_41 (25) = happyGoto action_54
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (117) = happyShift action_52
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (82) = happyShift action_49
action_43 (54) = happyGoto action_51
action_43 _ = happyReduce_128

action_44 (97) = happyShift action_50
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (79) = happyReduce_128
action_45 (82) = happyShift action_49
action_45 (54) = happyGoto action_48
action_45 _ = happyReduce_94

action_46 (111) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (112) = happyShift action_129
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (79) = happyShift action_128
action_48 (55) = happyGoto action_127
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (116) = happyShift action_95
action_49 (20) = happyGoto action_125
action_49 (21) = happyGoto action_126
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_88

action_51 (76) = happyShift action_67
action_51 (77) = happyShift action_68
action_51 (78) = happyShift action_69
action_51 (79) = happyShift action_70
action_51 (83) = happyShift action_71
action_51 (84) = happyShift action_72
action_51 (85) = happyShift action_73
action_51 (86) = happyShift action_74
action_51 (87) = happyShift action_75
action_51 (88) = happyShift action_76
action_51 (89) = happyShift action_77
action_51 (102) = happyShift action_78
action_51 (110) = happyShift action_79
action_51 (114) = happyShift action_80
action_51 (115) = happyShift action_81
action_51 (116) = happyShift action_2
action_51 (118) = happyShift action_82
action_51 (4) = happyGoto action_62
action_51 (29) = happyGoto action_124
action_51 (30) = happyGoto action_64
action_51 (31) = happyGoto action_65
action_51 (32) = happyGoto action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_14

action_53 _ = happyReduce_38

action_54 (90) = happyShift action_123
action_54 _ = happyReduce_28

action_55 (110) = happyShift action_55
action_55 (116) = happyShift action_56
action_55 (23) = happyGoto action_53
action_55 (24) = happyGoto action_121
action_55 (25) = happyGoto action_122
action_55 _ = happyReduce_35

action_56 _ = happyReduce_33

action_57 (113) = happyShift action_119
action_57 (116) = happyShift action_120
action_57 (17) = happyGoto action_118
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (112) = happyShift action_40
action_58 (15) = happyGoto action_117
action_58 _ = happyReduce_21

action_59 (107) = happyShift action_115
action_59 (113) = happyShift action_116
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_139

action_61 (108) = happyShift action_114
action_61 _ = happyReduce_141

action_62 _ = happyReduce_81

action_63 (91) = happyShift action_100
action_63 (92) = happyShift action_101
action_63 (93) = happyShift action_102
action_63 (94) = happyShift action_103
action_63 (95) = happyShift action_104
action_63 (96) = happyShift action_105
action_63 (97) = happyShift action_106
action_63 (98) = happyShift action_107
action_63 (99) = happyShift action_108
action_63 (101) = happyShift action_109
action_63 (102) = happyShift action_110
action_63 (103) = happyShift action_111
action_63 (104) = happyShift action_112
action_63 (105) = happyShift action_113
action_63 _ = happyReduce_135

action_64 (83) = happyShift action_71
action_64 (84) = happyShift action_72
action_64 (110) = happyShift action_79
action_64 (114) = happyShift action_80
action_64 (115) = happyShift action_81
action_64 (116) = happyShift action_2
action_64 (118) = happyShift action_82
action_64 (4) = happyGoto action_62
action_64 (31) = happyGoto action_99
action_64 (32) = happyGoto action_66
action_64 _ = happyReduce_72

action_65 (106) = happyShift action_98
action_65 _ = happyReduce_74

action_66 _ = happyReduce_76

action_67 (76) = happyShift action_67
action_67 (77) = happyShift action_68
action_67 (78) = happyShift action_69
action_67 (79) = happyShift action_70
action_67 (83) = happyShift action_71
action_67 (84) = happyShift action_72
action_67 (85) = happyShift action_73
action_67 (86) = happyShift action_74
action_67 (87) = happyShift action_75
action_67 (88) = happyShift action_76
action_67 (89) = happyShift action_77
action_67 (102) = happyShift action_78
action_67 (110) = happyShift action_79
action_67 (114) = happyShift action_80
action_67 (115) = happyShift action_81
action_67 (116) = happyShift action_2
action_67 (118) = happyShift action_82
action_67 (4) = happyGoto action_62
action_67 (29) = happyGoto action_97
action_67 (30) = happyGoto action_64
action_67 (31) = happyGoto action_65
action_67 (32) = happyGoto action_66
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (116) = happyShift action_95
action_68 (21) = happyGoto action_96
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (116) = happyShift action_95
action_69 (21) = happyGoto action_94
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (76) = happyShift action_67
action_70 (77) = happyShift action_68
action_70 (78) = happyShift action_69
action_70 (79) = happyShift action_70
action_70 (83) = happyShift action_71
action_70 (84) = happyShift action_72
action_70 (85) = happyShift action_73
action_70 (86) = happyShift action_74
action_70 (87) = happyShift action_75
action_70 (88) = happyShift action_76
action_70 (89) = happyShift action_77
action_70 (102) = happyShift action_78
action_70 (110) = happyShift action_79
action_70 (114) = happyShift action_80
action_70 (115) = happyShift action_81
action_70 (116) = happyShift action_2
action_70 (118) = happyShift action_82
action_70 (4) = happyGoto action_62
action_70 (29) = happyGoto action_93
action_70 (30) = happyGoto action_64
action_70 (31) = happyGoto action_65
action_70 (32) = happyGoto action_66
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_82

action_72 _ = happyReduce_83

action_73 (76) = happyShift action_67
action_73 (77) = happyShift action_68
action_73 (78) = happyShift action_69
action_73 (79) = happyShift action_70
action_73 (83) = happyShift action_71
action_73 (84) = happyShift action_72
action_73 (85) = happyShift action_73
action_73 (86) = happyShift action_74
action_73 (87) = happyShift action_75
action_73 (88) = happyShift action_76
action_73 (89) = happyShift action_77
action_73 (102) = happyShift action_78
action_73 (110) = happyShift action_79
action_73 (114) = happyShift action_80
action_73 (115) = happyShift action_81
action_73 (116) = happyShift action_2
action_73 (118) = happyShift action_82
action_73 (4) = happyGoto action_62
action_73 (29) = happyGoto action_92
action_73 (30) = happyGoto action_64
action_73 (31) = happyGoto action_65
action_73 (32) = happyGoto action_66
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (76) = happyShift action_67
action_74 (77) = happyShift action_68
action_74 (78) = happyShift action_69
action_74 (79) = happyShift action_70
action_74 (83) = happyShift action_71
action_74 (84) = happyShift action_72
action_74 (85) = happyShift action_73
action_74 (86) = happyShift action_74
action_74 (87) = happyShift action_75
action_74 (88) = happyShift action_76
action_74 (89) = happyShift action_77
action_74 (102) = happyShift action_78
action_74 (110) = happyShift action_79
action_74 (114) = happyShift action_80
action_74 (115) = happyShift action_81
action_74 (116) = happyShift action_2
action_74 (118) = happyShift action_82
action_74 (4) = happyGoto action_62
action_74 (29) = happyGoto action_91
action_74 (30) = happyGoto action_64
action_74 (31) = happyGoto action_65
action_74 (32) = happyGoto action_66
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (76) = happyShift action_67
action_75 (77) = happyShift action_68
action_75 (78) = happyShift action_69
action_75 (79) = happyShift action_70
action_75 (83) = happyShift action_71
action_75 (84) = happyShift action_72
action_75 (85) = happyShift action_73
action_75 (86) = happyShift action_74
action_75 (87) = happyShift action_75
action_75 (88) = happyShift action_76
action_75 (89) = happyShift action_77
action_75 (102) = happyShift action_78
action_75 (110) = happyShift action_79
action_75 (114) = happyShift action_80
action_75 (115) = happyShift action_81
action_75 (116) = happyShift action_2
action_75 (118) = happyShift action_82
action_75 (4) = happyGoto action_62
action_75 (29) = happyGoto action_90
action_75 (30) = happyGoto action_64
action_75 (31) = happyGoto action_65
action_75 (32) = happyGoto action_66
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (76) = happyShift action_67
action_76 (77) = happyShift action_68
action_76 (78) = happyShift action_69
action_76 (79) = happyShift action_70
action_76 (83) = happyShift action_71
action_76 (84) = happyShift action_72
action_76 (85) = happyShift action_73
action_76 (86) = happyShift action_74
action_76 (87) = happyShift action_75
action_76 (88) = happyShift action_76
action_76 (89) = happyShift action_77
action_76 (102) = happyShift action_78
action_76 (110) = happyShift action_79
action_76 (114) = happyShift action_80
action_76 (115) = happyShift action_81
action_76 (116) = happyShift action_2
action_76 (118) = happyShift action_82
action_76 (4) = happyGoto action_62
action_76 (29) = happyGoto action_89
action_76 (30) = happyGoto action_64
action_76 (31) = happyGoto action_65
action_76 (32) = happyGoto action_66
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (116) = happyShift action_88
action_77 (22) = happyGoto action_87
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (76) = happyShift action_67
action_78 (77) = happyShift action_68
action_78 (78) = happyShift action_69
action_78 (79) = happyShift action_70
action_78 (83) = happyShift action_71
action_78 (84) = happyShift action_72
action_78 (85) = happyShift action_73
action_78 (86) = happyShift action_74
action_78 (87) = happyShift action_75
action_78 (88) = happyShift action_76
action_78 (89) = happyShift action_77
action_78 (102) = happyShift action_78
action_78 (110) = happyShift action_79
action_78 (114) = happyShift action_80
action_78 (115) = happyShift action_81
action_78 (116) = happyShift action_2
action_78 (118) = happyShift action_82
action_78 (4) = happyGoto action_62
action_78 (29) = happyGoto action_86
action_78 (30) = happyGoto action_64
action_78 (31) = happyGoto action_65
action_78 (32) = happyGoto action_66
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (76) = happyShift action_67
action_79 (77) = happyShift action_68
action_79 (78) = happyShift action_69
action_79 (79) = happyShift action_70
action_79 (83) = happyShift action_71
action_79 (84) = happyShift action_72
action_79 (85) = happyShift action_73
action_79 (86) = happyShift action_74
action_79 (87) = happyShift action_75
action_79 (88) = happyShift action_76
action_79 (89) = happyShift action_77
action_79 (102) = happyShift action_78
action_79 (110) = happyShift action_79
action_79 (114) = happyShift action_80
action_79 (115) = happyShift action_81
action_79 (116) = happyShift action_2
action_79 (118) = happyShift action_82
action_79 (4) = happyGoto action_62
action_79 (29) = happyGoto action_84
action_79 (30) = happyGoto action_64
action_79 (31) = happyGoto action_65
action_79 (32) = happyGoto action_66
action_79 (33) = happyGoto action_85
action_79 _ = happyReduce_84

action_80 _ = happyReduce_78

action_81 _ = happyReduce_79

action_82 _ = happyReduce_80

action_83 _ = happyReduce_15

action_84 (91) = happyShift action_100
action_84 (92) = happyShift action_101
action_84 (93) = happyShift action_102
action_84 (94) = happyShift action_103
action_84 (95) = happyShift action_104
action_84 (96) = happyShift action_105
action_84 (97) = happyShift action_106
action_84 (98) = happyShift action_107
action_84 (99) = happyShift action_108
action_84 (101) = happyShift action_109
action_84 (102) = happyShift action_110
action_84 (103) = happyShift action_111
action_84 (104) = happyShift action_112
action_84 (105) = happyShift action_113
action_84 _ = happyReduce_85

action_85 (107) = happyShift action_167
action_85 (111) = happyShift action_168
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_68

action_87 (90) = happyShift action_166
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (108) = happyShift action_165
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (91) = happyShift action_100
action_89 (92) = happyShift action_101
action_89 (93) = happyShift action_102
action_89 (94) = happyShift action_103
action_89 (95) = happyShift action_104
action_89 (96) = happyShift action_105
action_89 (97) = happyShift action_106
action_89 (98) = happyShift action_107
action_89 (99) = happyShift action_108
action_89 (101) = happyShift action_109
action_89 (102) = happyShift action_110
action_89 (103) = happyShift action_111
action_89 (104) = happyShift action_112
action_89 (105) = happyShift action_113
action_89 _ = happyReduce_54

action_90 (91) = happyShift action_100
action_90 (92) = happyShift action_101
action_90 (93) = happyShift action_102
action_90 (94) = happyShift action_103
action_90 (95) = happyShift action_104
action_90 (96) = happyShift action_105
action_90 (97) = happyShift action_106
action_90 (98) = happyShift action_107
action_90 (99) = happyShift action_108
action_90 (101) = happyShift action_109
action_90 (102) = happyShift action_110
action_90 (103) = happyShift action_111
action_90 (104) = happyShift action_112
action_90 (105) = happyShift action_113
action_90 _ = happyReduce_53

action_91 (91) = happyShift action_100
action_91 (92) = happyShift action_101
action_91 (93) = happyShift action_102
action_91 (94) = happyShift action_103
action_91 (95) = happyShift action_104
action_91 (96) = happyShift action_105
action_91 (97) = happyShift action_106
action_91 (98) = happyShift action_107
action_91 (99) = happyShift action_108
action_91 (101) = happyShift action_109
action_91 (102) = happyShift action_110
action_91 (103) = happyShift action_111
action_91 (104) = happyShift action_112
action_91 (105) = happyShift action_113
action_91 _ = happyReduce_52

action_92 (91) = happyShift action_100
action_92 (92) = happyShift action_101
action_92 (93) = happyShift action_102
action_92 (94) = happyShift action_103
action_92 (95) = happyShift action_104
action_92 (96) = happyShift action_105
action_92 (97) = happyShift action_106
action_92 (98) = happyShift action_107
action_92 (99) = happyShift action_108
action_92 (101) = happyShift action_109
action_92 (102) = happyShift action_110
action_92 (103) = happyShift action_111
action_92 (104) = happyShift action_112
action_92 (105) = happyShift action_113
action_92 _ = happyReduce_51

action_93 (80) = happyShift action_164
action_93 (91) = happyShift action_100
action_93 (92) = happyShift action_101
action_93 (93) = happyShift action_102
action_93 (94) = happyShift action_103
action_93 (95) = happyShift action_104
action_93 (96) = happyShift action_105
action_93 (97) = happyShift action_106
action_93 (98) = happyShift action_107
action_93 (99) = happyShift action_108
action_93 (101) = happyShift action_109
action_93 (102) = happyShift action_110
action_93 (103) = happyShift action_111
action_93 (104) = happyShift action_112
action_93 (105) = happyShift action_113
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (106) = happyShift action_163
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (108) = happyShift action_162
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (106) = happyShift action_161
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (94) = happyShift action_103
action_97 (95) = happyShift action_104
action_97 (96) = happyShift action_105
action_97 (97) = happyShift action_106
action_97 (98) = happyShift action_107
action_97 (99) = happyShift action_108
action_97 (101) = happyShift action_109
action_97 (102) = happyShift action_110
action_97 (103) = happyShift action_111
action_97 (104) = happyShift action_112
action_97 (105) = happyShift action_113
action_97 _ = happyReduce_59

action_98 (116) = happyShift action_160
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (106) = happyShift action_98
action_99 _ = happyReduce_73

action_100 (76) = happyShift action_67
action_100 (77) = happyShift action_68
action_100 (78) = happyShift action_69
action_100 (79) = happyShift action_70
action_100 (83) = happyShift action_71
action_100 (84) = happyShift action_72
action_100 (85) = happyShift action_73
action_100 (86) = happyShift action_74
action_100 (87) = happyShift action_75
action_100 (88) = happyShift action_76
action_100 (89) = happyShift action_77
action_100 (102) = happyShift action_78
action_100 (110) = happyShift action_79
action_100 (114) = happyShift action_80
action_100 (115) = happyShift action_81
action_100 (116) = happyShift action_2
action_100 (118) = happyShift action_82
action_100 (4) = happyGoto action_62
action_100 (29) = happyGoto action_159
action_100 (30) = happyGoto action_64
action_100 (31) = happyGoto action_65
action_100 (32) = happyGoto action_66
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (76) = happyShift action_67
action_101 (77) = happyShift action_68
action_101 (78) = happyShift action_69
action_101 (79) = happyShift action_70
action_101 (83) = happyShift action_71
action_101 (84) = happyShift action_72
action_101 (85) = happyShift action_73
action_101 (86) = happyShift action_74
action_101 (87) = happyShift action_75
action_101 (88) = happyShift action_76
action_101 (89) = happyShift action_77
action_101 (102) = happyShift action_78
action_101 (110) = happyShift action_79
action_101 (114) = happyShift action_80
action_101 (115) = happyShift action_81
action_101 (116) = happyShift action_2
action_101 (118) = happyShift action_82
action_101 (4) = happyGoto action_62
action_101 (29) = happyGoto action_158
action_101 (30) = happyGoto action_64
action_101 (31) = happyGoto action_65
action_101 (32) = happyGoto action_66
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (76) = happyShift action_67
action_102 (77) = happyShift action_68
action_102 (78) = happyShift action_69
action_102 (79) = happyShift action_70
action_102 (83) = happyShift action_71
action_102 (84) = happyShift action_72
action_102 (85) = happyShift action_73
action_102 (86) = happyShift action_74
action_102 (87) = happyShift action_75
action_102 (88) = happyShift action_76
action_102 (89) = happyShift action_77
action_102 (102) = happyShift action_78
action_102 (110) = happyShift action_79
action_102 (114) = happyShift action_80
action_102 (115) = happyShift action_81
action_102 (116) = happyShift action_2
action_102 (118) = happyShift action_82
action_102 (4) = happyGoto action_62
action_102 (29) = happyGoto action_157
action_102 (30) = happyGoto action_64
action_102 (31) = happyGoto action_65
action_102 (32) = happyGoto action_66
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (76) = happyShift action_67
action_103 (77) = happyShift action_68
action_103 (78) = happyShift action_69
action_103 (79) = happyShift action_70
action_103 (83) = happyShift action_71
action_103 (84) = happyShift action_72
action_103 (85) = happyShift action_73
action_103 (86) = happyShift action_74
action_103 (87) = happyShift action_75
action_103 (88) = happyShift action_76
action_103 (89) = happyShift action_77
action_103 (102) = happyShift action_78
action_103 (110) = happyShift action_79
action_103 (114) = happyShift action_80
action_103 (115) = happyShift action_81
action_103 (116) = happyShift action_2
action_103 (118) = happyShift action_82
action_103 (4) = happyGoto action_62
action_103 (29) = happyGoto action_156
action_103 (30) = happyGoto action_64
action_103 (31) = happyGoto action_65
action_103 (32) = happyGoto action_66
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (76) = happyShift action_67
action_104 (77) = happyShift action_68
action_104 (78) = happyShift action_69
action_104 (79) = happyShift action_70
action_104 (83) = happyShift action_71
action_104 (84) = happyShift action_72
action_104 (85) = happyShift action_73
action_104 (86) = happyShift action_74
action_104 (87) = happyShift action_75
action_104 (88) = happyShift action_76
action_104 (89) = happyShift action_77
action_104 (102) = happyShift action_78
action_104 (110) = happyShift action_79
action_104 (114) = happyShift action_80
action_104 (115) = happyShift action_81
action_104 (116) = happyShift action_2
action_104 (118) = happyShift action_82
action_104 (4) = happyGoto action_62
action_104 (29) = happyGoto action_155
action_104 (30) = happyGoto action_64
action_104 (31) = happyGoto action_65
action_104 (32) = happyGoto action_66
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (76) = happyShift action_67
action_105 (77) = happyShift action_68
action_105 (78) = happyShift action_69
action_105 (79) = happyShift action_70
action_105 (83) = happyShift action_71
action_105 (84) = happyShift action_72
action_105 (85) = happyShift action_73
action_105 (86) = happyShift action_74
action_105 (87) = happyShift action_75
action_105 (88) = happyShift action_76
action_105 (89) = happyShift action_77
action_105 (102) = happyShift action_78
action_105 (110) = happyShift action_79
action_105 (114) = happyShift action_80
action_105 (115) = happyShift action_81
action_105 (116) = happyShift action_2
action_105 (118) = happyShift action_82
action_105 (4) = happyGoto action_62
action_105 (29) = happyGoto action_154
action_105 (30) = happyGoto action_64
action_105 (31) = happyGoto action_65
action_105 (32) = happyGoto action_66
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (76) = happyShift action_67
action_106 (77) = happyShift action_68
action_106 (78) = happyShift action_69
action_106 (79) = happyShift action_70
action_106 (83) = happyShift action_71
action_106 (84) = happyShift action_72
action_106 (85) = happyShift action_73
action_106 (86) = happyShift action_74
action_106 (87) = happyShift action_75
action_106 (88) = happyShift action_76
action_106 (89) = happyShift action_77
action_106 (102) = happyShift action_78
action_106 (110) = happyShift action_79
action_106 (114) = happyShift action_80
action_106 (115) = happyShift action_81
action_106 (116) = happyShift action_2
action_106 (118) = happyShift action_82
action_106 (4) = happyGoto action_62
action_106 (29) = happyGoto action_153
action_106 (30) = happyGoto action_64
action_106 (31) = happyGoto action_65
action_106 (32) = happyGoto action_66
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (76) = happyShift action_67
action_107 (77) = happyShift action_68
action_107 (78) = happyShift action_69
action_107 (79) = happyShift action_70
action_107 (83) = happyShift action_71
action_107 (84) = happyShift action_72
action_107 (85) = happyShift action_73
action_107 (86) = happyShift action_74
action_107 (87) = happyShift action_75
action_107 (88) = happyShift action_76
action_107 (89) = happyShift action_77
action_107 (102) = happyShift action_78
action_107 (110) = happyShift action_79
action_107 (114) = happyShift action_80
action_107 (115) = happyShift action_81
action_107 (116) = happyShift action_2
action_107 (118) = happyShift action_82
action_107 (4) = happyGoto action_62
action_107 (29) = happyGoto action_152
action_107 (30) = happyGoto action_64
action_107 (31) = happyGoto action_65
action_107 (32) = happyGoto action_66
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (76) = happyShift action_67
action_108 (77) = happyShift action_68
action_108 (78) = happyShift action_69
action_108 (79) = happyShift action_70
action_108 (83) = happyShift action_71
action_108 (84) = happyShift action_72
action_108 (85) = happyShift action_73
action_108 (86) = happyShift action_74
action_108 (87) = happyShift action_75
action_108 (88) = happyShift action_76
action_108 (89) = happyShift action_77
action_108 (102) = happyShift action_78
action_108 (110) = happyShift action_79
action_108 (114) = happyShift action_80
action_108 (115) = happyShift action_81
action_108 (116) = happyShift action_2
action_108 (118) = happyShift action_82
action_108 (4) = happyGoto action_62
action_108 (29) = happyGoto action_151
action_108 (30) = happyGoto action_64
action_108 (31) = happyGoto action_65
action_108 (32) = happyGoto action_66
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (76) = happyShift action_67
action_109 (77) = happyShift action_68
action_109 (78) = happyShift action_69
action_109 (79) = happyShift action_70
action_109 (83) = happyShift action_71
action_109 (84) = happyShift action_72
action_109 (85) = happyShift action_73
action_109 (86) = happyShift action_74
action_109 (87) = happyShift action_75
action_109 (88) = happyShift action_76
action_109 (89) = happyShift action_77
action_109 (102) = happyShift action_78
action_109 (110) = happyShift action_79
action_109 (114) = happyShift action_80
action_109 (115) = happyShift action_81
action_109 (116) = happyShift action_2
action_109 (118) = happyShift action_82
action_109 (4) = happyGoto action_62
action_109 (29) = happyGoto action_150
action_109 (30) = happyGoto action_64
action_109 (31) = happyGoto action_65
action_109 (32) = happyGoto action_66
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (76) = happyShift action_67
action_110 (77) = happyShift action_68
action_110 (78) = happyShift action_69
action_110 (79) = happyShift action_70
action_110 (83) = happyShift action_71
action_110 (84) = happyShift action_72
action_110 (85) = happyShift action_73
action_110 (86) = happyShift action_74
action_110 (87) = happyShift action_75
action_110 (88) = happyShift action_76
action_110 (89) = happyShift action_77
action_110 (102) = happyShift action_78
action_110 (110) = happyShift action_79
action_110 (114) = happyShift action_80
action_110 (115) = happyShift action_81
action_110 (116) = happyShift action_2
action_110 (118) = happyShift action_82
action_110 (4) = happyGoto action_62
action_110 (29) = happyGoto action_149
action_110 (30) = happyGoto action_64
action_110 (31) = happyGoto action_65
action_110 (32) = happyGoto action_66
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (76) = happyShift action_67
action_111 (77) = happyShift action_68
action_111 (78) = happyShift action_69
action_111 (79) = happyShift action_70
action_111 (83) = happyShift action_71
action_111 (84) = happyShift action_72
action_111 (85) = happyShift action_73
action_111 (86) = happyShift action_74
action_111 (87) = happyShift action_75
action_111 (88) = happyShift action_76
action_111 (89) = happyShift action_77
action_111 (102) = happyShift action_78
action_111 (110) = happyShift action_79
action_111 (114) = happyShift action_80
action_111 (115) = happyShift action_81
action_111 (116) = happyShift action_2
action_111 (118) = happyShift action_82
action_111 (4) = happyGoto action_62
action_111 (29) = happyGoto action_148
action_111 (30) = happyGoto action_64
action_111 (31) = happyGoto action_65
action_111 (32) = happyGoto action_66
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (76) = happyShift action_67
action_112 (77) = happyShift action_68
action_112 (78) = happyShift action_69
action_112 (79) = happyShift action_70
action_112 (83) = happyShift action_71
action_112 (84) = happyShift action_72
action_112 (85) = happyShift action_73
action_112 (86) = happyShift action_74
action_112 (87) = happyShift action_75
action_112 (88) = happyShift action_76
action_112 (89) = happyShift action_77
action_112 (102) = happyShift action_78
action_112 (110) = happyShift action_79
action_112 (114) = happyShift action_80
action_112 (115) = happyShift action_81
action_112 (116) = happyShift action_2
action_112 (118) = happyShift action_82
action_112 (4) = happyGoto action_62
action_112 (29) = happyGoto action_147
action_112 (30) = happyGoto action_64
action_112 (31) = happyGoto action_65
action_112 (32) = happyGoto action_66
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (76) = happyShift action_67
action_113 (77) = happyShift action_68
action_113 (78) = happyShift action_69
action_113 (79) = happyShift action_70
action_113 (83) = happyShift action_71
action_113 (84) = happyShift action_72
action_113 (85) = happyShift action_73
action_113 (86) = happyShift action_74
action_113 (87) = happyShift action_75
action_113 (88) = happyShift action_76
action_113 (89) = happyShift action_77
action_113 (102) = happyShift action_78
action_113 (110) = happyShift action_79
action_113 (114) = happyShift action_80
action_113 (115) = happyShift action_81
action_113 (116) = happyShift action_2
action_113 (118) = happyShift action_82
action_113 (4) = happyGoto action_62
action_113 (29) = happyGoto action_146
action_113 (30) = happyGoto action_64
action_113 (31) = happyGoto action_65
action_113 (32) = happyGoto action_66
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (83) = happyShift action_142
action_114 (84) = happyShift action_143
action_114 (112) = happyShift action_36
action_114 (114) = happyShift action_144
action_114 (116) = happyShift action_145
action_114 (59) = happyGoto action_141
action_114 _ = happyReduce_136

action_115 (116) = happyShift action_61
action_115 (61) = happyGoto action_140
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_137

action_117 _ = happyReduce_20

action_118 _ = happyReduce_24

action_119 _ = happyReduce_22

action_120 (108) = happyShift action_139
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (107) = happyShift action_137
action_121 (111) = happyShift action_138
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (90) = happyShift action_123
action_122 _ = happyReduce_36

action_123 (110) = happyShift action_55
action_123 (116) = happyShift action_56
action_123 (23) = happyGoto action_53
action_123 (25) = happyGoto action_136
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (91) = happyShift action_100
action_124 (92) = happyShift action_101
action_124 (93) = happyShift action_102
action_124 (94) = happyShift action_103
action_124 (95) = happyShift action_104
action_124 (96) = happyShift action_105
action_124 (97) = happyShift action_106
action_124 (98) = happyShift action_107
action_124 (99) = happyShift action_108
action_124 (101) = happyShift action_109
action_124 (102) = happyShift action_110
action_124 (103) = happyShift action_111
action_124 (104) = happyShift action_112
action_124 (105) = happyShift action_113
action_124 _ = happyReduce_96

action_125 (107) = happyShift action_135
action_125 _ = happyReduce_129

action_126 _ = happyReduce_29

action_127 (80) = happyShift action_134
action_127 (56) = happyGoto action_133
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (76) = happyShift action_67
action_128 (77) = happyShift action_68
action_128 (78) = happyShift action_69
action_128 (79) = happyShift action_70
action_128 (83) = happyShift action_71
action_128 (84) = happyShift action_72
action_128 (85) = happyShift action_73
action_128 (86) = happyShift action_74
action_128 (87) = happyShift action_75
action_128 (88) = happyShift action_76
action_128 (89) = happyShift action_77
action_128 (102) = happyShift action_78
action_128 (110) = happyShift action_79
action_128 (114) = happyShift action_80
action_128 (115) = happyShift action_81
action_128 (116) = happyShift action_2
action_128 (118) = happyShift action_82
action_128 (4) = happyGoto action_62
action_128 (29) = happyGoto action_132
action_128 (30) = happyGoto action_64
action_128 (31) = happyGoto action_65
action_128 (32) = happyGoto action_66
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (70) = happyShift action_131
action_129 (40) = happyGoto action_130
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (71) = happyShift action_183
action_130 (41) = happyGoto action_182
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (116) = happyShift action_181
action_131 (28) = happyGoto action_180
action_131 _ = happyReduce_45

action_132 (91) = happyShift action_100
action_132 (92) = happyShift action_101
action_132 (93) = happyShift action_102
action_132 (94) = happyShift action_103
action_132 (95) = happyShift action_104
action_132 (96) = happyShift action_105
action_132 (97) = happyShift action_106
action_132 (98) = happyShift action_107
action_132 (99) = happyShift action_108
action_132 (101) = happyShift action_109
action_132 (102) = happyShift action_110
action_132 (103) = happyShift action_111
action_132 (104) = happyShift action_112
action_132 (105) = happyShift action_113
action_132 _ = happyReduce_130

action_133 _ = happyReduce_95

action_134 (76) = happyShift action_67
action_134 (77) = happyShift action_68
action_134 (78) = happyShift action_69
action_134 (79) = happyShift action_70
action_134 (83) = happyShift action_71
action_134 (84) = happyShift action_72
action_134 (85) = happyShift action_73
action_134 (86) = happyShift action_74
action_134 (87) = happyShift action_75
action_134 (88) = happyShift action_76
action_134 (89) = happyShift action_77
action_134 (102) = happyShift action_78
action_134 (110) = happyShift action_79
action_134 (114) = happyShift action_80
action_134 (115) = happyShift action_81
action_134 (116) = happyShift action_2
action_134 (118) = happyShift action_82
action_134 (4) = happyGoto action_62
action_134 (29) = happyGoto action_179
action_134 (30) = happyGoto action_64
action_134 (31) = happyGoto action_65
action_134 (32) = happyGoto action_66
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (116) = happyShift action_95
action_135 (21) = happyGoto action_178
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (90) = happyShift action_123
action_136 _ = happyReduce_39

action_137 (110) = happyShift action_55
action_137 (116) = happyShift action_56
action_137 (23) = happyGoto action_53
action_137 (25) = happyGoto action_177
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_34

action_139 (110) = happyShift action_55
action_139 (116) = happyShift action_56
action_139 (23) = happyGoto action_53
action_139 (25) = happyGoto action_176
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_140

action_141 _ = happyReduce_146

action_142 _ = happyReduce_143

action_143 _ = happyReduce_144

action_144 _ = happyReduce_145

action_145 _ = happyReduce_142

action_146 _ = happyReduce_71

action_147 _ = happyReduce_70

action_148 _ = happyReduce_69

action_149 (103) = happyShift action_111
action_149 (104) = happyShift action_112
action_149 (105) = happyShift action_113
action_149 _ = happyReduce_67

action_150 (103) = happyShift action_111
action_150 (104) = happyShift action_112
action_150 (105) = happyShift action_113
action_150 _ = happyReduce_66

action_151 (94) = happyFail []
action_151 (95) = happyFail []
action_151 (96) = happyFail []
action_151 (97) = happyFail []
action_151 (98) = happyFail []
action_151 (99) = happyFail []
action_151 (101) = happyShift action_109
action_151 (102) = happyShift action_110
action_151 (103) = happyShift action_111
action_151 (104) = happyShift action_112
action_151 (105) = happyShift action_113
action_151 _ = happyReduce_65

action_152 (94) = happyFail []
action_152 (95) = happyFail []
action_152 (96) = happyFail []
action_152 (97) = happyFail []
action_152 (98) = happyFail []
action_152 (99) = happyFail []
action_152 (101) = happyShift action_109
action_152 (102) = happyShift action_110
action_152 (103) = happyShift action_111
action_152 (104) = happyShift action_112
action_152 (105) = happyShift action_113
action_152 _ = happyReduce_63

action_153 (94) = happyFail []
action_153 (95) = happyFail []
action_153 (96) = happyFail []
action_153 (97) = happyFail []
action_153 (98) = happyFail []
action_153 (99) = happyFail []
action_153 (101) = happyShift action_109
action_153 (102) = happyShift action_110
action_153 (103) = happyShift action_111
action_153 (104) = happyShift action_112
action_153 (105) = happyShift action_113
action_153 _ = happyReduce_62

action_154 (94) = happyFail []
action_154 (95) = happyFail []
action_154 (96) = happyFail []
action_154 (97) = happyFail []
action_154 (98) = happyFail []
action_154 (99) = happyFail []
action_154 (101) = happyShift action_109
action_154 (102) = happyShift action_110
action_154 (103) = happyShift action_111
action_154 (104) = happyShift action_112
action_154 (105) = happyShift action_113
action_154 _ = happyReduce_61

action_155 (94) = happyFail []
action_155 (95) = happyFail []
action_155 (96) = happyFail []
action_155 (97) = happyFail []
action_155 (98) = happyFail []
action_155 (99) = happyFail []
action_155 (101) = happyShift action_109
action_155 (102) = happyShift action_110
action_155 (103) = happyShift action_111
action_155 (104) = happyShift action_112
action_155 (105) = happyShift action_113
action_155 _ = happyReduce_60

action_156 (94) = happyFail []
action_156 (95) = happyFail []
action_156 (96) = happyFail []
action_156 (97) = happyFail []
action_156 (98) = happyFail []
action_156 (99) = happyFail []
action_156 (101) = happyShift action_109
action_156 (102) = happyShift action_110
action_156 (103) = happyShift action_111
action_156 (104) = happyShift action_112
action_156 (105) = happyShift action_113
action_156 _ = happyReduce_64

action_157 (93) = happyShift action_102
action_157 (94) = happyShift action_103
action_157 (95) = happyShift action_104
action_157 (96) = happyShift action_105
action_157 (97) = happyShift action_106
action_157 (98) = happyShift action_107
action_157 (99) = happyShift action_108
action_157 (101) = happyShift action_109
action_157 (102) = happyShift action_110
action_157 (103) = happyShift action_111
action_157 (104) = happyShift action_112
action_157 (105) = happyShift action_113
action_157 _ = happyReduce_57

action_158 (92) = happyShift action_101
action_158 (93) = happyShift action_102
action_158 (94) = happyShift action_103
action_158 (95) = happyShift action_104
action_158 (96) = happyShift action_105
action_158 (97) = happyShift action_106
action_158 (98) = happyShift action_107
action_158 (99) = happyShift action_108
action_158 (101) = happyShift action_109
action_158 (102) = happyShift action_110
action_158 (103) = happyShift action_111
action_158 (104) = happyShift action_112
action_158 (105) = happyShift action_113
action_158 _ = happyReduce_56

action_159 (91) = happyShift action_100
action_159 (92) = happyShift action_101
action_159 (93) = happyShift action_102
action_159 (94) = happyShift action_103
action_159 (95) = happyShift action_104
action_159 (96) = happyShift action_105
action_159 (97) = happyShift action_106
action_159 (98) = happyShift action_107
action_159 (99) = happyShift action_108
action_159 (101) = happyShift action_109
action_159 (102) = happyShift action_110
action_159 (103) = happyShift action_111
action_159 (104) = happyShift action_112
action_159 (105) = happyShift action_113
action_159 _ = happyReduce_55

action_160 _ = happyReduce_75

action_161 (76) = happyShift action_67
action_161 (77) = happyShift action_68
action_161 (78) = happyShift action_69
action_161 (79) = happyShift action_70
action_161 (83) = happyShift action_71
action_161 (84) = happyShift action_72
action_161 (85) = happyShift action_73
action_161 (86) = happyShift action_74
action_161 (87) = happyShift action_75
action_161 (88) = happyShift action_76
action_161 (89) = happyShift action_77
action_161 (102) = happyShift action_78
action_161 (110) = happyShift action_79
action_161 (114) = happyShift action_80
action_161 (115) = happyShift action_81
action_161 (116) = happyShift action_2
action_161 (118) = happyShift action_82
action_161 (4) = happyGoto action_62
action_161 (29) = happyGoto action_175
action_161 (30) = happyGoto action_64
action_161 (31) = happyGoto action_65
action_161 (32) = happyGoto action_66
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (110) = happyShift action_55
action_162 (116) = happyShift action_56
action_162 (23) = happyGoto action_53
action_162 (25) = happyGoto action_174
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (76) = happyShift action_67
action_163 (77) = happyShift action_68
action_163 (78) = happyShift action_69
action_163 (79) = happyShift action_70
action_163 (83) = happyShift action_71
action_163 (84) = happyShift action_72
action_163 (85) = happyShift action_73
action_163 (86) = happyShift action_74
action_163 (87) = happyShift action_75
action_163 (88) = happyShift action_76
action_163 (89) = happyShift action_77
action_163 (102) = happyShift action_78
action_163 (110) = happyShift action_79
action_163 (114) = happyShift action_80
action_163 (115) = happyShift action_81
action_163 (116) = happyShift action_2
action_163 (118) = happyShift action_82
action_163 (4) = happyGoto action_62
action_163 (29) = happyGoto action_173
action_163 (30) = happyGoto action_64
action_163 (31) = happyGoto action_65
action_163 (32) = happyGoto action_66
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (76) = happyShift action_67
action_164 (77) = happyShift action_68
action_164 (78) = happyShift action_69
action_164 (79) = happyShift action_70
action_164 (83) = happyShift action_71
action_164 (84) = happyShift action_72
action_164 (85) = happyShift action_73
action_164 (86) = happyShift action_74
action_164 (87) = happyShift action_75
action_164 (88) = happyShift action_76
action_164 (89) = happyShift action_77
action_164 (102) = happyShift action_78
action_164 (110) = happyShift action_79
action_164 (114) = happyShift action_80
action_164 (115) = happyShift action_81
action_164 (116) = happyShift action_2
action_164 (118) = happyShift action_82
action_164 (4) = happyGoto action_62
action_164 (29) = happyGoto action_172
action_164 (30) = happyGoto action_64
action_164 (31) = happyGoto action_65
action_164 (32) = happyGoto action_66
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (110) = happyShift action_55
action_165 (116) = happyShift action_56
action_165 (23) = happyGoto action_171
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (76) = happyShift action_67
action_166 (77) = happyShift action_68
action_166 (78) = happyShift action_69
action_166 (79) = happyShift action_70
action_166 (83) = happyShift action_71
action_166 (84) = happyShift action_72
action_166 (85) = happyShift action_73
action_166 (86) = happyShift action_74
action_166 (87) = happyShift action_75
action_166 (88) = happyShift action_76
action_166 (89) = happyShift action_77
action_166 (102) = happyShift action_78
action_166 (110) = happyShift action_79
action_166 (114) = happyShift action_80
action_166 (115) = happyShift action_81
action_166 (116) = happyShift action_2
action_166 (118) = happyShift action_82
action_166 (4) = happyGoto action_62
action_166 (29) = happyGoto action_170
action_166 (30) = happyGoto action_64
action_166 (31) = happyGoto action_65
action_166 (32) = happyGoto action_66
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (76) = happyShift action_67
action_167 (77) = happyShift action_68
action_167 (78) = happyShift action_69
action_167 (79) = happyShift action_70
action_167 (83) = happyShift action_71
action_167 (84) = happyShift action_72
action_167 (85) = happyShift action_73
action_167 (86) = happyShift action_74
action_167 (87) = happyShift action_75
action_167 (88) = happyShift action_76
action_167 (89) = happyShift action_77
action_167 (102) = happyShift action_78
action_167 (110) = happyShift action_79
action_167 (114) = happyShift action_80
action_167 (115) = happyShift action_81
action_167 (116) = happyShift action_2
action_167 (118) = happyShift action_82
action_167 (4) = happyGoto action_62
action_167 (29) = happyGoto action_169
action_167 (30) = happyGoto action_64
action_167 (31) = happyGoto action_65
action_167 (32) = happyGoto action_66
action_167 _ = happyFail (happyExpListPerState 167)

action_168 _ = happyReduce_77

action_169 (91) = happyShift action_100
action_169 (92) = happyShift action_101
action_169 (93) = happyShift action_102
action_169 (94) = happyShift action_103
action_169 (95) = happyShift action_104
action_169 (96) = happyShift action_105
action_169 (97) = happyShift action_106
action_169 (98) = happyShift action_107
action_169 (99) = happyShift action_108
action_169 (101) = happyShift action_109
action_169 (102) = happyShift action_110
action_169 (103) = happyShift action_111
action_169 (104) = happyShift action_112
action_169 (105) = happyShift action_113
action_169 _ = happyReduce_86

action_170 (91) = happyShift action_100
action_170 (92) = happyShift action_101
action_170 (93) = happyShift action_102
action_170 (94) = happyShift action_103
action_170 (95) = happyShift action_104
action_170 (96) = happyShift action_105
action_170 (97) = happyShift action_106
action_170 (98) = happyShift action_107
action_170 (99) = happyShift action_108
action_170 (101) = happyShift action_109
action_170 (102) = happyShift action_110
action_170 (103) = happyShift action_111
action_170 (104) = happyShift action_112
action_170 (105) = happyShift action_113
action_170 _ = happyReduce_48

action_171 _ = happyReduce_32

action_172 (81) = happyShift action_191
action_172 (91) = happyShift action_100
action_172 (92) = happyShift action_101
action_172 (93) = happyShift action_102
action_172 (94) = happyShift action_103
action_172 (95) = happyShift action_104
action_172 (96) = happyShift action_105
action_172 (97) = happyShift action_106
action_172 (98) = happyShift action_107
action_172 (99) = happyShift action_108
action_172 (101) = happyShift action_109
action_172 (102) = happyShift action_110
action_172 (103) = happyShift action_111
action_172 (104) = happyShift action_112
action_172 (105) = happyShift action_113
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (91) = happyShift action_100
action_173 (92) = happyShift action_101
action_173 (93) = happyShift action_102
action_173 (94) = happyShift action_103
action_173 (95) = happyShift action_104
action_173 (96) = happyShift action_105
action_173 (97) = happyShift action_106
action_173 (98) = happyShift action_107
action_173 (99) = happyShift action_108
action_173 (101) = happyShift action_109
action_173 (102) = happyShift action_110
action_173 (103) = happyShift action_111
action_173 (104) = happyShift action_112
action_173 (105) = happyShift action_113
action_173 _ = happyReduce_50

action_174 (90) = happyShift action_123
action_174 _ = happyReduce_31

action_175 (91) = happyShift action_100
action_175 (92) = happyShift action_101
action_175 (93) = happyShift action_102
action_175 (94) = happyShift action_103
action_175 (95) = happyShift action_104
action_175 (96) = happyShift action_105
action_175 (97) = happyShift action_106
action_175 (98) = happyShift action_107
action_175 (99) = happyShift action_108
action_175 (101) = happyShift action_109
action_175 (102) = happyShift action_110
action_175 (103) = happyShift action_111
action_175 (104) = happyShift action_112
action_175 (105) = happyShift action_113
action_175 _ = happyReduce_49

action_176 (90) = happyShift action_123
action_176 _ = happyReduce_25

action_177 (90) = happyShift action_123
action_177 _ = happyReduce_37

action_178 _ = happyReduce_30

action_179 (91) = happyShift action_100
action_179 (92) = happyShift action_101
action_179 (93) = happyShift action_102
action_179 (94) = happyShift action_103
action_179 (95) = happyShift action_104
action_179 (96) = happyShift action_105
action_179 (97) = happyShift action_106
action_179 (98) = happyShift action_107
action_179 (99) = happyShift action_108
action_179 (101) = happyShift action_109
action_179 (102) = happyShift action_110
action_179 (103) = happyShift action_111
action_179 (104) = happyShift action_112
action_179 (105) = happyShift action_113
action_179 _ = happyReduce_131

action_180 (107) = happyShift action_189
action_180 (109) = happyShift action_190
action_180 _ = happyFail (happyExpListPerState 180)

action_181 _ = happyReduce_46

action_182 (72) = happyShift action_188
action_182 (46) = happyGoto action_187
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (116) = happyShift action_186
action_183 (42) = happyGoto action_184
action_183 (43) = happyGoto action_185
action_183 _ = happyReduce_100

action_184 (107) = happyShift action_198
action_184 (109) = happyShift action_199
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_101

action_186 (112) = happyShift action_197
action_186 _ = happyReduce_103

action_187 (73) = happyShift action_196
action_187 (47) = happyGoto action_195
action_187 _ = happyFail (happyExpListPerState 187)

action_188 (116) = happyShift action_194
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (116) = happyShift action_193
action_189 _ = happyFail (happyExpListPerState 189)

action_190 _ = happyReduce_98

action_191 (76) = happyShift action_67
action_191 (77) = happyShift action_68
action_191 (78) = happyShift action_69
action_191 (79) = happyShift action_70
action_191 (83) = happyShift action_71
action_191 (84) = happyShift action_72
action_191 (85) = happyShift action_73
action_191 (86) = happyShift action_74
action_191 (87) = happyShift action_75
action_191 (88) = happyShift action_76
action_191 (89) = happyShift action_77
action_191 (102) = happyShift action_78
action_191 (110) = happyShift action_79
action_191 (114) = happyShift action_80
action_191 (115) = happyShift action_81
action_191 (116) = happyShift action_2
action_191 (118) = happyShift action_82
action_191 (4) = happyGoto action_62
action_191 (29) = happyGoto action_192
action_191 (30) = happyGoto action_64
action_191 (31) = happyGoto action_65
action_191 (32) = happyGoto action_66
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (91) = happyShift action_100
action_192 (92) = happyShift action_101
action_192 (93) = happyShift action_102
action_192 (94) = happyShift action_103
action_192 (95) = happyShift action_104
action_192 (96) = happyShift action_105
action_192 (97) = happyShift action_106
action_192 (98) = happyShift action_107
action_192 (99) = happyShift action_108
action_192 (101) = happyShift action_109
action_192 (102) = happyShift action_110
action_192 (103) = happyShift action_111
action_192 (104) = happyShift action_112
action_192 (105) = happyShift action_113
action_192 _ = happyReduce_58

action_193 _ = happyReduce_47

action_194 (109) = happyShift action_208
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (113) = happyShift action_207
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (116) = happyShift action_206
action_196 (48) = happyGoto action_204
action_196 (49) = happyGoto action_205
action_196 _ = happyReduce_116

action_197 (116) = happyShift action_203
action_197 (44) = happyGoto action_201
action_197 (45) = happyGoto action_202
action_197 _ = happyReduce_105

action_198 (116) = happyShift action_186
action_198 (43) = happyGoto action_200
action_198 _ = happyFail (happyExpListPerState 198)

action_199 _ = happyReduce_99

action_200 _ = happyReduce_102

action_201 (93) = happyShift action_218
action_201 (113) = happyShift action_219
action_201 _ = happyFail (happyExpListPerState 201)

action_202 _ = happyReduce_106

action_203 (94) = happyShift action_212
action_203 (95) = happyShift action_213
action_203 (96) = happyShift action_214
action_203 (97) = happyShift action_215
action_203 (98) = happyShift action_216
action_203 (99) = happyShift action_217
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (107) = happyShift action_210
action_204 (109) = happyShift action_211
action_204 _ = happyFail (happyExpListPerState 204)

action_205 _ = happyReduce_117

action_206 (90) = happyShift action_209
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_97

action_208 _ = happyReduce_114

action_209 (116) = happyShift action_228
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (116) = happyShift action_206
action_210 (49) = happyGoto action_227
action_210 _ = happyFail (happyExpListPerState 210)

action_211 _ = happyReduce_115

action_212 (114) = happyShift action_226
action_212 _ = happyFail (happyExpListPerState 212)

action_213 (114) = happyShift action_225
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (114) = happyShift action_224
action_214 _ = happyFail (happyExpListPerState 214)

action_215 (114) = happyShift action_223
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (114) = happyShift action_222
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (114) = happyShift action_221
action_217 _ = happyFail (happyExpListPerState 217)

action_218 (116) = happyShift action_203
action_218 (45) = happyGoto action_220
action_218 _ = happyFail (happyExpListPerState 218)

action_219 _ = happyReduce_104

action_220 _ = happyReduce_107

action_221 _ = happyReduce_113

action_222 _ = happyReduce_111

action_223 _ = happyReduce_110

action_224 _ = happyReduce_109

action_225 _ = happyReduce_108

action_226 _ = happyReduce_112

action_227 _ = happyReduce_118

action_228 (112) = happyShift action_229
action_228 _ = happyFail (happyExpListPerState 228)

action_229 (74) = happyShift action_231
action_229 (50) = happyGoto action_230
action_229 _ = happyReduce_120

action_230 (75) = happyShift action_234
action_230 (51) = happyGoto action_233
action_230 _ = happyReduce_122

action_231 (116) = happyShift action_203
action_231 (44) = happyGoto action_232
action_231 (45) = happyGoto action_202
action_231 _ = happyReduce_105

action_232 (93) = happyShift action_218
action_232 (109) = happyShift action_239
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (113) = happyShift action_238
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (116) = happyShift action_237
action_234 (52) = happyGoto action_235
action_234 (53) = happyGoto action_236
action_234 _ = happyReduce_124

action_235 (107) = happyShift action_241
action_235 (109) = happyShift action_242
action_235 _ = happyFail (happyExpListPerState 235)

action_236 _ = happyReduce_125

action_237 (100) = happyShift action_240
action_237 _ = happyFail (happyExpListPerState 237)

action_238 _ = happyReduce_119

action_239 _ = happyReduce_121

action_240 (114) = happyShift action_244
action_240 _ = happyFail (happyExpListPerState 240)

action_241 (116) = happyShift action_237
action_241 (53) = happyGoto action_243
action_241 _ = happyFail (happyExpListPerState 241)

action_242 _ = happyReduce_123

action_243 _ = happyReduce_126

action_244 _ = happyReduce_127

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 (QVarName (getLoc happy_var_1) (tokenSym happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (NewProgram (tokenRangeList (map getLoc happy_var_1)) (reverse happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (map MappingTLE happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (ClassDeclTLE happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn8
		 (VarDeclTLE happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn8
		 (RuleTLE happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn8
		 (AssertionTLE happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn8
		 (AutomatonTLE happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  9 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 10 happyReduction_14
happyReduction_14 ((HappyTerminal happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Mapping (tokenRange happy_var_1 happy_var_4) (tokenSym happy_var_2) (parseDescription $ tokenStringLit happy_var_4)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (Mapping (tokenRange happy_var_1 happy_var_3) (tokenSym happy_var_1) (parseDescription $ tokenStringLit happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  12 happyReduction_16
happyReduction_16  =  HappyAbsSyn12
		 ([]
	)

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_1
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (case snd happy_var_3 of
                                       -- ClassDef is empty, so take token range of 'class VAR'
                                       Nothing ->  if tokenSym happy_var_2 == "Object"
                                                   -- special treatment: create Object class without superclass
                                                   then ClassDecl (tokenRange happy_var_1 happy_var_2) (ClsNm $ tokenSym happy_var_2) (ClassDef [] [])
                                                   -- take default class created in first component of happy_var_3
                                                   else ClassDecl (tokenRange happy_var_1 happy_var_2) (ClsNm $ tokenSym happy_var_2) (fst happy_var_3)
                                       -- ClassDef is non-empty, but the data type ClassDef has no position annotation,
                                       -- so retrieve position info from production ClassDef
                                       Just rng -> ClassDecl (coordFromTo (getLoc happy_var_1) rng) (ClsNm $ tokenSym happy_var_2) (fst happy_var_3)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ((ClassDef [ClsNm "Class"] (reverse (fst happy_var_1)), (snd happy_var_1))
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (case snd happy_var_3 of
                 Nothing -> (ClassDef [ClsNm $ tokenSym happy_var_2] (reverse (fst happy_var_3)), Just (tokenRange happy_var_1 happy_var_2))
                 Just rng -> (ClassDef [ClsNm $ tokenSym happy_var_2] (reverse (fst happy_var_3)), Just (coordFromTo (getLoc happy_var_1) rng ))
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  15 happyReduction_21
happyReduction_21  =  HappyAbsSyn15
		 (([], Nothing)
	)

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 ((happy_var_2, Just (tokenRange happy_var_1 happy_var_3))
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  16 happyReduction_23
happyReduction_23  =  HappyAbsSyn16
		 ([]
	)

happyReduce_24 = happySpecReduce_2  16 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn25  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (FieldDecl (tokenRange happy_var_1 happy_var_3) (FldNm $ tokenSym happy_var_1) happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  18 happyReduction_26
happyReduction_26  =  HappyAbsSyn18
		 ([]
	)

happyReduce_27 = happySpecReduce_2  18 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 19 happyReduction_28
happyReduction_28 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (VarDecl (tokenRange happy_var_1 happy_var_4) (tokenSym happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  20 happyReduction_29
happyReduction_29 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  20 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  21 happyReduction_31
happyReduction_31 (HappyAbsSyn25  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (VarDecl (tokenRange happy_var_1 happy_var_3) (tokenSym happy_var_1) happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  22 happyReduction_32
happyReduction_32 (HappyAbsSyn23  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (VarDecl (tokenRange happy_var_1 happy_var_3) (tokenSym happy_var_1) happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  23 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (ClassT (getLoc happy_var_1) (ClsNm $ tokenSym happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  23 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_3)
	(HappyAbsSyn24  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (case happy_var_2 of [t] -> t{annotOfTp = (tokenRange happy_var_1 happy_var_3)}; tcs -> TupleT (tokenRange happy_var_1 happy_var_3) (reverse tcs)
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  24 happyReduction_35
happyReduction_35  =  HappyAbsSyn24
		 ([]
	)

happyReduce_36 = happySpecReduce_1  24 happyReduction_36
happyReduction_36 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  24 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_3 : happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  25 happyReduction_38
happyReduction_38 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  25 happyReduction_39
happyReduction_39 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (FunT (tokenRange happy_var_1 happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  26 happyReduction_40
happyReduction_40 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn26
		 (VarP happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  26 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (let vcs = happy_var_2 in if length vcs == 1 then VarP (head vcs) else VarListP (reverse vcs)
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  27 happyReduction_42
happyReduction_42  =  HappyAbsSyn27
		 ([]
	)

happyReduce_43 = happySpecReduce_1  27 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  27 happyReduction_44
happyReduction_44 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_3 : happy_var_1
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  28 happyReduction_45
happyReduction_45  =  HappyAbsSyn28
		 ([]
	)

happyReduce_46 = happySpecReduce_1  28 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ([tokenSym happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  28 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 ((tokenSym happy_var_3) : happy_var_1
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 29 happyReduction_48
happyReduction_48 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (FunE (tokenRange happy_var_1 happy_var_4) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 4 29 happyReduction_49
happyReduction_49 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (QuantifE (tokenRange happy_var_1 happy_var_4) All happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 29 happyReduction_50
happyReduction_50 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (QuantifE (tokenRange happy_var_1 happy_var_4) Ex  happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_2  29 happyReduction_51
happyReduction_51 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UTemporal UTAF) happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  29 happyReduction_52
happyReduction_52 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UTemporal UTAG) happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  29 happyReduction_53
happyReduction_53 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UTemporal UTEF) happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  29 happyReduction_54
happyReduction_54 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UTemporal UTEG) happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  29 happyReduction_55
happyReduction_55 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BBool BBimpl) happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  29 happyReduction_56
happyReduction_56 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BBool BBor) happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  29 happyReduction_57
happyReduction_57 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BBool BBand) happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 6 29 happyReduction_58
happyReduction_58 ((HappyAbsSyn29  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (IfThenElseE (tokenRange happy_var_1 happy_var_6) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_2  29 happyReduction_59
happyReduction_59 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UBool UBnot) happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  29 happyReduction_60
happyReduction_60 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BClt) happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  29 happyReduction_61
happyReduction_61 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BClte) happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  29 happyReduction_62
happyReduction_62 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BCgt) happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  29 happyReduction_63
happyReduction_63 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BCgte) happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  29 happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BCeq) happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  29 happyReduction_65
happyReduction_65 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BCompar BCne) happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  29 happyReduction_66
happyReduction_66 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BArith BAadd) happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  29 happyReduction_67
happyReduction_67 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BArith BAsub) happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  29 happyReduction_68
happyReduction_68 (HappyAbsSyn29  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (UnaOpE (tokenRange happy_var_1 happy_var_2) (UArith UAminus) happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  29 happyReduction_69
happyReduction_69 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BArith BAmul) happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  29 happyReduction_70
happyReduction_70 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BArith BAdiv) happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  29 happyReduction_71
happyReduction_71 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (BinOpE (tokenRange happy_var_1 happy_var_3) (BArith BAmod) happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  29 happyReduction_72
happyReduction_72 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  30 happyReduction_73
happyReduction_73 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (AppE (tokenRange happy_var_1 happy_var_2) happy_var_1 happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  30 happyReduction_74
happyReduction_74 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  31 happyReduction_75
happyReduction_75 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (FldAccE (tokenRange happy_var_1 happy_var_3) happy_var_1 (FldNm $ tokenSym happy_var_3)
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  31 happyReduction_76
happyReduction_76 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  32 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (let ecs = happy_var_2
                                     in
                                        if length ecs == 1
                                        then updAnnotOfExpr (const (tokenRange happy_var_1 happy_var_3)) (head ecs)
                                        else TupleE (tokenRange happy_var_1 happy_var_3) (reverse ecs)
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  32 happyReduction_78
happyReduction_78 (HappyTerminal (L pos (TokenInteger happy_var_1)))
	 =  HappyAbsSyn32
		 (ValE (pos) (IntV happy_var_1)
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  32 happyReduction_79
happyReduction_79 (HappyTerminal (L pos (TokenFloat happy_var_1)))
	 =  HappyAbsSyn32
		 (ValE (pos) (FloatV happy_var_1)
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (ValE (tokenPos happy_var_1) (StringV (tokenString happy_var_1))
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn32
		 (VarE (getLoc happy_var_1) (GlobalVar happy_var_1)
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (ValE (tokenPos happy_var_1) (BoolV True)
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  32 happyReduction_83
happyReduction_83 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (ValE (tokenPos happy_var_1) (BoolV False)
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  33 happyReduction_84
happyReduction_84  =  HappyAbsSyn33
		 ([]
	)

happyReduce_85 = happySpecReduce_1  33 happyReduction_85
happyReduction_85 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  33 happyReduction_86
happyReduction_86 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_3 : happy_var_1
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_0  34 happyReduction_87
happyReduction_87  =  HappyAbsSyn34
		 (Nothing
	)

happyReduce_88 = happySpecReduce_3  34 happyReduction_88
happyReduction_88 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn34
		 (Just (tokenSym happy_var_2)
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_0  35 happyReduction_89
happyReduction_89  =  HappyAbsSyn35
		 ([]
	)

happyReduce_90 = happySpecReduce_2  35 happyReduction_90
happyReduction_90 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_2 : happy_var_1
	)
happyReduction_90 _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2  35 happyReduction_91
happyReduction_91 (HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (happy_var_2 : happy_var_1
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  36 happyReduction_92
happyReduction_92 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  36 happyReduction_93
happyReduction_93 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  37 happyReduction_94
happyReduction_94 (HappyAbsSyn59  happy_var_3)
	(HappyAbsSyn34  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (Rule (getLoc happy_var_1) happy_var_2 happy_var_3  [] (ValE (nullSRng) (BoolV True)) (ValE (nullSRng) (BoolV True))
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happyReduce 6 37 happyReduction_95
happyReduction_95 ((HappyAbsSyn56  happy_var_6) `HappyStk`
	(HappyAbsSyn55  happy_var_5) `HappyStk`
	(HappyAbsSyn54  happy_var_4) `HappyStk`
	(HappyAbsSyn59  happy_var_3) `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 (Rule (tokenRange happy_var_1 happy_var_6) happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_96 = happyReduce 5 38 happyReduction_96
happyReduction_96 ((HappyAbsSyn29  happy_var_5) `HappyStk`
	(HappyAbsSyn54  happy_var_4) `HappyStk`
	(HappyAbsSyn59  happy_var_3) `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (Rule (tokenRange happy_var_1 happy_var_5) happy_var_2 happy_var_3 happy_var_4 (ValE (nullSRng) (BoolV True)) happy_var_5
	) `HappyStk` happyRest

happyReduce_97 = happyReduce 10 39 happyReduction_97
happyReduction_97 ((HappyTerminal happy_var_10) `HappyStk`
	(HappyAbsSyn47  happy_var_9) `HappyStk`
	(HappyAbsSyn46  happy_var_8) `HappyStk`
	(HappyAbsSyn41  happy_var_7) `HappyStk`
	(HappyAbsSyn40  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn39
		 (TA {annotOfTA = (tokenRange happy_var_1 happy_var_10),
        nameOfTA = (tokenSym happy_var_2), locsOfTA = (map fst (reverse happy_var_7)), channelsOfTA = [], clocksOfTA = reverse happy_var_6,
        transitionsOfTA = reverse happy_var_9, initialLocOfTA = happy_var_8, invarsOfTA = (reverse happy_var_7), labellingOfTA = []}
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_3  40 happyReduction_98
happyReduction_98 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (map Clock happy_var_2
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  41 happyReduction_99
happyReduction_99 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (happy_var_2
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_0  42 happyReduction_100
happyReduction_100  =  HappyAbsSyn42
		 ([]
	)

happyReduce_101 = happySpecReduce_1  42 happyReduction_101
happyReduction_101 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn42
		 ([happy_var_1]
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  42 happyReduction_102
happyReduction_102 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_3 : happy_var_1
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  43 happyReduction_103
happyReduction_103 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn43
		 ((Loc (tokenSym happy_var_1), [])
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happyReduce 4 43 happyReduction_104
happyReduction_104 (_ `HappyStk`
	(HappyAbsSyn44  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn43
		 ((Loc (tokenSym happy_var_1), happy_var_3)
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_0  44 happyReduction_105
happyReduction_105  =  HappyAbsSyn44
		 ([]
	)

happyReduce_106 = happySpecReduce_1  44 happyReduction_106
happyReduction_106 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  44 happyReduction_107
happyReduction_107 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_3 : happy_var_1
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3  45 happyReduction_108
happyReduction_108 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BClt happy_var_3
	)
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  45 happyReduction_109
happyReduction_109 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BClte happy_var_3
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  45 happyReduction_110
happyReduction_110 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BCgt happy_var_3
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_3  45 happyReduction_111
happyReduction_111 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BCgte happy_var_3
	)
happyReduction_111 _ _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  45 happyReduction_112
happyReduction_112 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BCeq happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  45 happyReduction_113
happyReduction_113 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (ClConstr (Clock (tokenSym happy_var_1)) BCne happy_var_3
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3  46 happyReduction_114
happyReduction_114 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Loc (tokenSym happy_var_2)
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  47 happyReduction_115
happyReduction_115 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn47
		 (happy_var_2
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_0  48 happyReduction_116
happyReduction_116  =  HappyAbsSyn48
		 ([]
	)

happyReduce_117 = happySpecReduce_1  48 happyReduction_117
happyReduction_117 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  48 happyReduction_118
happyReduction_118 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_3 : happy_var_1
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happyReduce 7 49 happyReduction_119
happyReduction_119 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_6) `HappyStk`
	(HappyAbsSyn50  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (Transition {sourceOfTransition = (Loc (tokenSym happy_var_1)),
		   	       guardOfTransition = happy_var_5,
			       actionOfTransition = happy_var_6,
			       targetOfTransition = (Loc (tokenSym happy_var_3))}
	) `HappyStk` happyRest

happyReduce_120 = happySpecReduce_0  50 happyReduction_120
happyReduction_120  =  HappyAbsSyn50
		 (TransitionGuard [] (ValE (nullSRng) (BoolV True))
	)

happyReduce_121 = happySpecReduce_3  50 happyReduction_121
happyReduction_121 _
	(HappyAbsSyn44  happy_var_2)
	_
	 =  HappyAbsSyn50
		 (TransitionGuard happy_var_2 (ValE (nullSRng) (BoolV True))
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_0  51 happyReduction_122
happyReduction_122  =  HappyAbsSyn51
		 (TransitionAction Internal [] (Skip (nullSRng))
	)

happyReduce_123 = happySpecReduce_3  51 happyReduction_123
happyReduction_123 _
	(HappyAbsSyn52  happy_var_2)
	_
	 =  HappyAbsSyn51
		 (TransitionAction Internal happy_var_2 (Skip (nullSRng))
	)
happyReduction_123 _ _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_0  52 happyReduction_124
happyReduction_124  =  HappyAbsSyn52
		 ([]
	)

happyReduce_125 = happySpecReduce_1  52 happyReduction_125
happyReduction_125 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn52
		 ([happy_var_1]
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_3  52 happyReduction_126
happyReduction_126 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_3 : happy_var_1
	)
happyReduction_126 _ _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  53 happyReduction_127
happyReduction_127 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn53
		 (Clock (tokenSym happy_var_1)
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_0  54 happyReduction_128
happyReduction_128  =  HappyAbsSyn54
		 ([]
	)

happyReduce_129 = happySpecReduce_2  54 happyReduction_129
happyReduction_129 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (reverse happy_var_2
	)
happyReduction_129 _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_2  55 happyReduction_130
happyReduction_130 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn55
		 (happy_var_2
	)
happyReduction_130 _ _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_2  56 happyReduction_131
happyReduction_131 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn56
		 (happy_var_2
	)
happyReduction_131 _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_0  57 happyReduction_132
happyReduction_132  =  HappyAbsSyn57
		 ([]
	)

happyReduce_133 = happySpecReduce_2  57 happyReduction_133
happyReduction_133 (HappyAbsSyn58  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (happy_var_2 : happy_var_1
	)
happyReduction_133 _ _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_3  58 happyReduction_134
happyReduction_134 (HappyAbsSyn59  happy_var_3)
	(HappyAbsSyn34  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn58
		 (Assertion (getLoc happy_var_1) happy_var_2 happy_var_3 (ValE (nullSRng) (BoolV True))
	)
happyReduction_134 _ _ _  = notHappyAtAll 

happyReduce_135 = happyReduce 4 58 happyReduction_135
happyReduction_135 ((HappyAbsSyn29  happy_var_4) `HappyStk`
	(HappyAbsSyn59  happy_var_3) `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn58
		 (Assertion (tokenRange happy_var_1 happy_var_4) happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_136 = happySpecReduce_0  59 happyReduction_136
happyReduction_136  =  HappyAbsSyn59
		 ([]
	)

happyReduce_137 = happySpecReduce_3  59 happyReduction_137
happyReduction_137 _
	(HappyAbsSyn60  happy_var_2)
	_
	 =  HappyAbsSyn59
		 (reverse happy_var_2
	)
happyReduction_137 _ _ _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_0  60 happyReduction_138
happyReduction_138  =  HappyAbsSyn60
		 ([]
	)

happyReduce_139 = happySpecReduce_1  60 happyReduction_139
happyReduction_139 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn60
		 ([happy_var_1]
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3  60 happyReduction_140
happyReduction_140 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (happy_var_3 : happy_var_1
	)
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  61 happyReduction_141
happyReduction_141 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, MapVM [])
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  61 happyReduction_142
happyReduction_142 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, IdVM $ tokenSym happy_var_3)
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_3  61 happyReduction_143
happyReduction_143 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, BoolVM True)
	)
happyReduction_143 _ _ _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_3  61 happyReduction_144
happyReduction_144 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, BoolVM False)
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_3  61 happyReduction_145
happyReduction_145 (HappyTerminal (L pos (TokenInteger happy_var_3)))
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, IntVM happy_var_3)
	)
happyReduction_145 _ _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_3  61 happyReduction_146
happyReduction_146 (HappyAbsSyn59  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn61
		 ((tokenSym happy_var_1, MapVM happy_var_3)
	)
happyReduction_146 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L _ TokenEOF -> action 119 119 tk (HappyState action) sts stk;
	L _ TokenAssert -> cont 62;
	L _ TokenClass -> cont 63;
	L _ TokenDecl -> cont 64;
	L _ TokenExtends -> cont 65;
	L _ TokenLexicon -> cont 66;
	L _ TokenFact -> cont 67;
	L _ TokenRule -> cont 68;
	L _ TokenProcess -> cont 69;
	L _ TokenClock -> cont 70;
	L _ TokenState -> cont 71;
	L _ TokenInit -> cont 72;
	L _ TokenTrans -> cont 73;
	L _ TokenGuard -> cont 74;
	L _ TokenAssign -> cont 75;
	L _ TokenNot -> cont 76;
	L _ TokenForall -> cont 77;
	L _ TokenExists -> cont 78;
	L _ TokenIf -> cont 79;
	L _ TokenThen -> cont 80;
	L _ TokenElse -> cont 81;
	L _ TokenFor -> cont 82;
	L _ TokenTrue -> cont 83;
	L _ TokenFalse -> cont 84;
	L _ TokenAF -> cont 85;
	L _ TokenAG -> cont 86;
	L _ TokenEF -> cont 87;
	L _ TokenEG -> cont 88;
	L _ TokenLambda -> cont 89;
	L _ TokenArrow -> cont 90;
	L _ TokenImpl -> cont 91;
	L _ TokenOr -> cont 92;
	L _ TokenAnd -> cont 93;
	L _ TokenEq -> cont 94;
	L _ TokenLt -> cont 95;
	L _ TokenLte -> cont 96;
	L _ TokenGt -> cont 97;
	L _ TokenGte -> cont 98;
	L _ TokenNe -> cont 99;
	L _ TokenAssignTo -> cont 100;
	L _ TokenAdd -> cont 101;
	L _ TokenSub -> cont 102;
	L _ TokenMul -> cont 103;
	L _ TokenDiv -> cont 104;
	L _ TokenMod -> cont 105;
	L _ TokenDot -> cont 106;
	L _ TokenComma -> cont 107;
	L _ TokenColon -> cont 108;
	L _ TokenSemicolon -> cont 109;
	L _ TokenLParen -> cont 110;
	L _ TokenRParen -> cont 111;
	L _ TokenLBrace -> cont 112;
	L _ TokenRBrace -> cont 113;
	L pos (TokenInteger happy_dollar_dollar) -> cont 114;
	L pos (TokenFloat happy_dollar_dollar) -> cont 115;
	L _ (TokenSym _) -> cont 116;
	L _ (TokenStringLit _) -> cont 117;
	L _ (TokenString _) -> cont 118;
	_ -> happyError' (tk, [])
	})

happyError_ explist 119 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
program = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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

--parseProgram :: FilePath -> String -> Either Err (Program SRng)
--parseProgram fp inp = mapRight newProgramToProgram (parseNewProgram fp inp)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
