--# -path=.:present

concrete PropEng of Prop = AtomsEng ** PropI - [PNeg] with
  (Syntax = SyntaxEng),
  (Symbolic = SymbolicEng),
  (Sentence = SentenceEng),
  (Extend = ExtendEng),
  (Verb = VerbEng)
   ** open (P = ParadigmsEng), (ExtraEng=ExtraEng), Prelude in {

-- exceptions

lin
  PNeg p = {
    s = mkS ExtraEng.UncNeg (mkCl
          (mkVP (mkNP the_Quant (mkCN case_N (mkAdv that_Subj p.s))))) ;
    c = False ---- ?
    } ;

-- instance of interface

oper
  case_N = P.mkN "case" ;
  such_A = P.mkA "such" ;
  then_Adv = P.mkAdv "then" ;
  element_N = P.mkN "element" ;
  set_N2 = P.mkN2 (P.mkN "set") ;
  hold_V = P.mkV "hold" "held" "held" ;

  singular = P.singular ; ---

-- test lexicon

lin
  -- Vertical = mkAP (P.mkA "vertical") ;
  -- Horizontal = mkAP (P.mkA "horizontal") ;
  -- Parallel = P.mkA2 (P.mkA "parallel") to_Prep ;
  -- Equal = P.mkA2 (P.mkA "equal") to_Prep ;
  Centre = mkFun1 "centre" ;
  Intersection = mkFun2 "intersection" ;

  Set k = k ** {s = mkCN set_N2 (mkNP a_Art plNum k.s)} ;
  KFun = funType (P.mkN3 function_N from_Prep to_Prep) ;

  -- Even = mkAP (P.mkA "even") ;
  -- Odd = mkAP (P.mkA "odd") ;
  Square = mkFun1 "square" ;
  Sum = mkFun2 "sum" ;
  Product = mkFun2 "product" ;
  Nat = mkKind (P.mkN "number") ;
  Boolean = mkKind (mkCN (P.mkA "Boolean") (P.mkN "value")) ;

oper

  function_N : N = P.mkN "function" ;
  other_A : A = P.mkA "other" ;
  type_N : N = P.mkN "type" ;

  mkFun1, mkFun2 : Str -> {s : Symb ; v : N2} = \s ->
    {s = mkSymb  ("\\" + s) ; v = P.mkN2 (P.mkN s)} ;

}
