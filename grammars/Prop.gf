abstract Prop = Atoms ** {

flags startcat = Prop ;

cat
  Prop ;
  PropAtom ;
  Pred1 ;
  Pred2 ;
  Ind ;
  Var ;
  Fun1 ;
  Fun2 ;
  Conj ;

  -- For lexicon
  Noun ;

fun
  PAtom  : PropAtom  -> Prop ;
  PNeg   : Prop  -> Prop ;
  PConj  : Conj  -> Prop -> Prop -> Prop ;
  PImpl  : Prop  -> Prop -> Prop ;

  PIfThenElse : Prop -> Prop -> Prop -> Prop ;

  PUniv  : Var -> Prop -> Prop ;
  PExist : Var -> Prop -> Prop ;

  IVar   : Var -> Ind ;
  IVarN  : Noun -> Ind ;
  -- IVarA  : Adj -> Ind ;

  APred1 : Pred1 -> Ind -> PropAtom ;
  APred2 : Pred2 -> Ind -> Ind -> PropAtom ;

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

  PNegAtom  : PropAtom -> Prop ;

  ConjPred1 : Conj -> [Pred1] -> Pred1 ;

  APredColl : Pred2 -> [Ind] -> PropAtom ;

  APredRefl : Pred2 -> Ind -> PropAtom ;

  IFunC  : Fun2 -> [Ind] -> Ind ;

  AKind  : Kind  -> Ind -> PropAtom ;

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
  AtomPred2 : Atom -> Pred2 ;
  AtomPred1 : Atom -> Pred1 ;
  AtomKind  : Atom -> Kind ;
  AtomNoun  : Atom -> Noun ;

  KNoun        : Quantifier -> Noun -> Kind ;
  KInd         : Ind -> Kind ;
  KFun         : Kind -> Kind -> Kind ;

  INoun : Quantifier -> Noun -> Ind ;


-- Quantifiers, to handle "a buyer / other buyer", or "first, second and third buyer"
-- instead of "buyers A, B and C"
cat
  Quantifier ;
fun
  Other : Quantifier ;
  First, Second, Third, Fourth : Quantifier ;
  QString : String -> Quantifier ;
  NoQuant : Quantifier -> Quantifier ;
}
