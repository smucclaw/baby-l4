--# -path=.:present

incomplete concrete PropI of Prop = open 
  Syntax, 
  Symbolic, 
  Sentence, ---- ExtAdvS
  WordNet,
  Extend,
  Prelude in {

lincat
  Prop = {s : S ; c : Bool} ; -- c = True for connectives
  Atom = {s : NP ; vp : VPS} ;
  Pred1 = VPS ;
  Pred2 = VPS2 ;
  Var = Symb ;
  Conj = {s : Syntax.Conj ; c : S} ;  -- s = and ; c = all these hold
  Ind  = {s : NP ; isSymbolic : Bool} ;
  Fun1 = {s : Symb ; v : N2} ;
  Fun2 = {s : Symb ; v : N2} ;
  Noun = N ;
  Adj = A ;
  Adj2 = A2 ;
  Verb = V ;
  Verb2 = V2 ;

oper
  -- TODO: add ReflVPS2 to Extend
  -- TODO: Add this function to Extend
  a2slash : A2 -> VPSlash = \a2 ->
    let vp : VP = mkVP (mkAP a2) ;
        dummyVPS : VPSlash = mkVPSlash WordNet.abandon_1_V2 ; -- random V2 from WN
    in dummyVPS **  -- has necessary fields for VPSlash
             vp **  -- has all the right fields except for c2
              {c2 = a2.c2} ; -- has the right c3

lin
  PAtom a = {s = PredVPS a.s a.vp ; c = False} ;
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
  APred2 f x y = {s = x.s ; vp = ComplVPS2 f y.s} ;

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
  [Pred1] = [VPS] ;
  [Ind] = [NP] ;
  [Var] = NP ;

oper
 myVPS : VP -> Extend.VPS = \vp -> Extend.MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
 myVPS2 : VPSlash -> Extend.VPS2 = \vp -> Extend.MkVPS2 (mkTemp presentTense simultaneousAnt) positivePol vp ;

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
    s = mkS (mkCl (mkNP a_Quant (mkCN (mkCN k.s vs) (mkAP (mkAP such_A) p.s)))) ;
    c = False
    } ;
  -- PNegAtom a = {
  --   s = mkS negativePol a ;
  --   c = False
  --   } ;

  BaseProp p q = {s = mkListS p.s q.s ; c = orB p.c q.c} ;
  ConsProp p ps = {s = mkListS p.s ps.s ; c = orB p.c ps.c} ;

  BaseVar x = (symb x) ;
  ConsVar x xs = mkNP and_Conj (mkListNP (symb x.s) xs) ;

  BaseInd x y = mkListNP x.s y.s ;
  ConsInd x xs = mkListNP x.s xs ;

  BasePred1 = BaseVPS ;
  ConsPred1 = ConsVPS ;

-- The non-chosen field is just some random word from WordNet
  PAdj1 a = myVPS (mkVP a) ;
  PAdj2 a = myVPS2 (a2slash a) ;
--  PAdj12 a = myVPS2 (mkVP a) ;
  PVerb1 v = myVPS (mkVP v) ;
  PVerb2 v = myVPS2 (mkVPSlash v) ;

lin
  ConjPred1 c = ConjVPS c.s ;

  APredColl f ps = {s = mkNP and_Conj ps ; vp = ComplVPS2 f it_NP} ; -- TODO empty NP
  APredRefl f x = {s = <x.s : NP> ; vp = ComplVPS2 f it_NP} ; -- TODO: actual reflexive

  IFunC f xs = {s = app f.v (mkNP and_Conj xs) ; isSymbolic = False} ;

  IUniv k = {s = mkNP every_Det k.s ; isSymbolic = False} ;
  IExist k = {s = mkNP someSg_Det k.s ; isSymbolic = False} ;

  ConjInd co xs = {s = mkNP co.s xs ; isSymbolic = False} ;

  -- ModKind k m = k ** {s = mkCN m k.s} ;

  PartPred f x = ComplVPS2 f x.s ; 

  IInt i = {s = symb i.s ; isSymbolic = True} ;

  BTrue = {s = symb "true" ; isSymbolic = True} ;
  BFalse = {s = symb "false" ; isSymbolic = True} ;
  KInd ind = {s = mkCN type_5_N ind.s ; isClass = True} ;
  KNoun noun = {s = mkCN noun ; isClass = False} ;
-- symbolic applications by LaTeX macros

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
