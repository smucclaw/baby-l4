abstract Prop = {

flags startcat = Prop ;

cat
  Prop ;
  Atom ;
  Pred1 ;
  Pred2 ;
  Ind ;
  Var ;
  Fun1 ;
  Fun2 ;
  Conj ;
  -- For lexicon
  -- fdjkjkv 
  Noun ; Noun2 ; Adj ; Adj2 ; Verb ; Verb2 ; PassVerb2 ;

fun
  PAtom  : Atom  -> Prop ;
  PNeg   : Prop  -> Prop ;
  PConj  : Conj  -> Prop -> Prop -> Prop ;
  PImpl  : Prop  -> Prop -> Prop ;

  PIfThenElse : Prop -> Prop -> Prop -> Prop ;

  PUniv  : Var -> Prop -> Prop ;
  PExist : Var -> Prop -> Prop ;

  IVar   : Var -> Ind ;
  IVarN  : Noun -> Ind ;
  -- IVarA  : Adj -> Ind ;

  APred1 : Pred1 -> Ind -> Atom ;
  APred2 : Pred2 -> Ind -> Ind -> Atom ;

  IFun1  : Fun1 -> Ind -> Ind ;
  IFun2  : Fun2 -> Ind -> Ind -> Ind ;

  VString : String -> Var ;

  CAnd, COr : Conj ;

-- supplementary

cat
  Kind ;
  [Prop] {2} ;
  [Var] {1} ;
  [Ind] {2} ;
  [Pred1] {2} ;

fun
  PConjs  : Conj  -> [Prop] -> Prop ;
  PUnivs  : [Var] -> Kind -> Prop -> Prop ;
  PExists : [Var] -> Kind -> Prop -> Prop ;
  PNotExists : [Var] -> Kind -> Prop -> Prop ;

  PNegAtom  : Atom -> Prop ;

  ConjPred1 : Conj -> [Pred1] -> Pred1 ;

  APredColl : Pred2 -> [Ind] -> Atom ;

  APredRefl : Pred2 -> Ind -> Atom ;

  IFunC  : Fun2 -> [Ind] -> Ind ;

  AKind  : Kind  -> Ind -> Atom ;

  IUniv  : Kind -> Ind ;
  IExist : Kind -> Ind ;

  ConjInd : Conj -> [Ind] -> Ind ;

  ModKind : Kind -> Pred1 -> Kind ;

  PartPred : Pred2 -> Ind -> Pred1 ; -- partial application: equal -> equal to y

-- test lexicon: geometry

fun
  Vertical, Horizontal : Pred1 ;
  Parallel, Equal : Pred2 ;
  Centre : Fun1 ;
  Intersection : Fun2 ;  

  Set : Kind -> Kind ;

-- test lexicon: arithmetic

  Even, Odd    : Pred1 ;
  Nat          : Kind ;
  Boolean      : Kind ;    

  Square       : Fun1 ;
  Sum, Product : Fun2 ;
  IInt         : Int -> Ind ;
  BTrue, BFalse : Ind ;

  ----------------------------
  -- Not part of the original

  -- Overgenerating, but we're using this grammar only to linearise
  KNoun        : Noun -> Kind ;
  KInd         : Ind -> Kind ;
  KFun         : Kind -> Kind -> Kind ;

  PAdj1 : Adj -> Pred1 ;
  PAdj2 : Adj2 -> Pred2 ;
  PAdj12 : Adj -> Pred2 ; -- force A to A2
  PNoun1 : Noun -> Pred1 ;
  PNoun2 : Noun2 -> Pred2 ;
  PVerb1 : Verb -> Pred1 ;
  PVerb2 : Verb2 -> Pred2 ;
  PPassV2 : PassVerb2 -> Pred1 ;
  Passive : Verb2 -> PassVerb2 ;

  -- Fallback: if word not in lexicon, make it into var
  PVar1 : Var -> Pred1 ;
  PVar2 : Var -> Pred2 ;


-- dummy instance of noun so that generated Prop.hs doesn't complain
  DummyN : Noun ;
  DummyN2 : Noun2 ;
  DummyA : Adj ;
  DummyA2 : Adj2 ;
  DummyV : Verb ;
  DummyV2 : Verb2 ;

}
