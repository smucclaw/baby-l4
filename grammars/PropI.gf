--# -path=.:present

incomplete concrete PropI of Prop = open 
  Syntax, 
  Symbolic, 
  Sentence, ---- ExtAdvS
  WordNet,
  Extend,
  Verb,
  Prelude in {

lincat
  Prop = {s : S ; c : Bool} ; -- c = True for connectives
  Atom = {s : NP ; vp : MyPol => VPS} ;
  Pred1 = MyPol => VPS ;
  Pred2 = MyPol => VPS2 ;
  Var = Symb ;
  Conj = {s : Syntax.Conj ; c : S} ;  -- s = and ; c = all these hold
  Ind  = {s : NP ; isSymbolic : Bool} ;
  Fun1 = {s : Symb ; v : N2} ;
  Fun2 = {s : Symb ; v : N2} ;
  Noun = N ;
  Noun2 = N2 ;
  Adj = A ;
  Adj2 = A2 ;
  Verb = V ;
  Verb2 = V2 ;
  PassVerb2 = VP ;

  Quantifier = {s : AP ; qt : QType} ;

param
  MyPol = MyPos | MyNeg ;
  QType = QQuant | QStr ;

lin
  PAtom a = {s = PredVPS a.s (a.vp ! MyPos) ; c = False} ;
  PNeg p = { 
    s = mkS negativePol (mkCl 
          (mkVP (mkNP the_Quant (mkCN case_N (mkAdv that_Subj p.s))))) ; 
    c = False ---- ?
    } ;
  PConj c p q = {s = mkS c.s p.s q.s ; c = True} ; -- can be ambiguous; cf. PConjs
  PImpl p q = {s = ExtAdvS (mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = True} ;
  PUniv v p = {
    s = ExtAdvS (mkAdv for_Prep (mkNP all_Predet (symb v.s))) p.s ;
    c = False
    } ;
  PExist v p = { 
    s = mkS (mkCl (mkNP a_Quant (mkCN (mkCN element_N (symb v.s)) 
            (mkAP (mkAP such_A) p.s)))) ;
    c = False
    } ;
  APred1 f x = {s = x.s ; vp = f} ;
  APred2 f x y = {s = x.s ; vp = \\pol => ComplVPS2 (f ! pol) y.s} ;

  IVar x = {s = (symb x) ; isSymbolic = True} ;
  IVarN n = {s = mkNP n ; isSymbolic = False} ;

  IFun1 f x = {
    s = case x.isSymbolic of {
          True  => app1 f.s x.s | app f.v x.s ; -- preferred symbolic, allowed verbal
          False => app f.v x.s
          } ;
    isSymbolic = x.isSymbolic
    } ;

  IFun2 f x y = {
    s = case <x.isSymbolic,y.isSymbolic> of {
          <True,True> => app2 f.s x.s y.s | app f.v x.s y.s ;
          _ => app f.v x.s y.s
          } ;
    isSymbolic = x.isSymbolic
    } ;

  VString s = mkSymb s.s ;

  CAnd = {s = and_Conj ; c = mkS (mkCl (mkNP all_Predet these_NP) hold_V)} ;
  COr = {
    s = or_Conj ; 
    c = mkS (mkCl (mkNP (mkNP (mkDet (mkCard at_least_AdN (mkCard "1")))) (mkAdv part_Prep these_NP)) hold_V)
    } ;

-- supplementary

lincat
  Kind = {s : CN ; isClass : Bool} ;
  [Prop] = {s : [S] ; c : Bool} ; -- c = True if any of props is complex
  [Pred1] = MyPol => [VPS] ;
  [Ind] = [NP] ;
  [Var] = NP ;

oper
  myVPS : VP -> MyPol => Extend.VPS = \vp ->
    let vps : Pol -> VPS = \pol -> Extend.MkVPS (mkTemp presentTense simultaneousAnt) pol vp ;
     in table { MyPos => vps positivePol ;
                MyNeg => vps negativePol } ;   
  myVPS2 : VPSlash -> MyPol => Extend.VPS2 = \vp -> 
    let vps2 : Pol -> VPS2 = \pol -> Extend.MkVPS2 (mkTemp presentTense simultaneousAnt) pol vp ;
     in table { MyPos => vps2 positivePol ;
                MyNeg => vps2 negativePol } ;  

lin
  AKind k x = 
    {s = x.s ;
     vp = case k.isClass of {
            True => myVPS (mkVP have_V2 (mkNP k.s)) ;
            False => myVPS (mkVP k.s)}
    } ;

  PConjs c ps = case ps.c of {
    True  => {s = mkS <colonConj : Conj> c.c (mkS <bulletConj : Conj> ps.s) ; c = False} ; ----
    False => {s = mkS c.s ps.s ; c = True}
    } ;
  PUnivs vs k p = {
    s = ExtAdvS (mkAdv for_Prep (mkNP all_Predet (mkNP aPl_Det (mkCN k.s vs)))) p.s ;
    c = False
    } ;
  PExists vs k p = {
    s = mkS (mkCl (mkNP aPl_Det (mkCN (mkCN k.s vs) (mkAP (mkAP such_A) p.s)))) ;
    c = False
    } ;
  PNotExists vs k p = {
    s = mkS negativePol (ExistCN (mkCN (mkCN k.s vs) (mkAP (mkAP such_A) p.s))) ;
    c = False
    } ;
  PNegAtom a = {
    s = PredVPS a.s (a.vp ! MyNeg) ;
    c = False
    } ;

  BaseProp p q = {s = mkListS p.s q.s ; c = orB p.c q.c} ;
  ConsProp p ps = {s = mkListS p.s ps.s ; c = orB p.c ps.c} ;

  BaseVar x = (symb x) ;
  ConsVar x xs = mkNP and_Conj (mkListNP (symb x.s) xs) ;

  BaseInd x y = mkListNP x.s y.s ;
  ConsInd x xs = mkListNP x.s xs ;

  BasePred1 p q = \\pol => BaseVPS (p ! pol) (q ! pol) ;
  ConsPred1 p ps = \\pol => ConsVPS (p ! pol) (ps ! pol) ;

  PAdj1 a = myVPS (mkVP a) ;
  PAdj2 a = myVPS2 (A2VPSlash a) ;
--  PAdj12 a = myVPS2 (mkVP a) ;
  PNoun1 n = myVPS (mkVP n) ;
  PNoun2 n = myVPS2 (N2VPSlash n) ;
  PVerb1 v = myVPS (mkVP v) ;
  PVerb2 v = myVPS2 (mkVPSlash v) ;
  Passive = passiveVP ;
  PPassV2 vp = myVPS vp ;

  PVar1 var = myVPS (mkVP (symb var)) ;
  PVar2 var = myVPS2 (VPSlashPrep (mkVP (symb var)) to_Prep) ; ----

lin
  ConjPred1 c ps = \\pol => ConjVPS c.s (ps ! pol) ;

  APredColl f ps = {s = mkNP and_Conj ps ; vp = \\pol => ComplVPS2 (f ! pol) it_NP} ; -- TODO empty NP
  APredRefl f x = {s = <x.s : NP> ; vp = \\pol => ReflVPS2 (f ! pol) ReflPron} ;

  IFunC f xs = {s = app f.v (mkNP and_Conj xs) ; isSymbolic = False} ;

  IUniv k = {s = mkNP every_Det k.s ; isSymbolic = False} ;
  IExist k = {s = mkNP aSg_Det k.s ; isSymbolic = False} ;

  ConjInd co xs = {s = mkNP co.s xs ; isSymbolic = False} ;

  -- ModKind k m = k ** {s = mkCN m k.s} ;

  PartPred f x = \\pol => ComplVPS2 (f ! pol) x.s ; 

  IInt i = {s = symb i.s ; isSymbolic = True} ;

  BTrue = {s = symb "true" ; isSymbolic = True} ;
  BFalse = {s = symb "false" ; isSymbolic = True} ;
  KInd ind = {s = mkCN type_5_N ind.s ; isClass = True} ;

  KNoun qnt noun = {
    s = case qnt.qt of {
          QStr => mkCN noun ; -- or mkCN noun (symb <string from the qnt>) to get "player A"
          QQuant => mkCN qnt.s noun } ;
    isClass = False
    } ;

  INoun qnt noun = {
    s = case qnt.qt of {
          QStr => mkNP the_Det noun ; -- or mkCN noun (symb <string from the qnt>) to get "player A"
          QQuant => mkNP (mkCN qnt.s noun) } ; 
    isSymbolic = False
    } ;

  First = {s = mkAP (mkOrd (mkNumeral n1_Unit)) ; qt = QQuant} ;
  Second = {s = mkAP (mkOrd (mkNumeral n2_Unit)) ; qt = QQuant} ;
  Third = {s = mkAP (mkOrd (mkNumeral n3_Unit)) ; qt = QQuant} ;
  Fourth = {s = mkAP (mkOrd (mkNumeral n4_Unit)) ; qt = QQuant} ;
  Other = {s = mkAP other_1_A ; qt = QQuant} ;
  QString _ = {s = mkAP other_1_A ; qt = QStr} ;

oper
  funType : N3 -> LinKind -> LinKind -> LinKind = \f,arg,ret ->
    {s = mkCN f (mkNP the_Det arg.s) (mkNP a_Det ret.s) ;
     isClass = False} ;

  app1 : Symb -> NP -> NP = \f,x -> symbNP (f.s ++ "{" ++ (mkUtt x).s ++ "}") ; 
  app2 : Symb -> NP -> NP -> NP = \f,x,y -> 
    symbNP (f.s ++ "{" ++ (mkUtt x).s ++ "}" ++ "{" ++ (mkUtt y).s ++ "}") ; 

  symbNP : Str -> NP = \s -> (symb (mkSymb s)) ;

  LinKind : Type = {s : CN ; isClass : Bool} ;
  mkKind = overload {

    mkKind : N -> LinKind = \n -> {
      s = mkCN n ; isClass = False } ;
    mkKind : CN -> LinKind = \cn -> {
      s = cn ; isClass = False }
    } ;

--- abuse of Conj category and its accidentally shared implementation

  bulletConj = lin Conj {s1,s2 = "\\item" ; n = singular ; isDiscont = True} ;
  colonConj = lin Conj {s1 = [] ; s2 = ":" ; n = singular ; isDiscont = False} ;


}
