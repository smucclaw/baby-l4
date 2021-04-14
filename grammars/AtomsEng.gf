concrete AtomsEng of Atoms = open Prelude, SyntaxEng, ExtendEng, (S=SyntaxEng), (P=ParadigmsEng) in {
  lincat
    Atom = LinAtom ;
    Pred = LinPred ;

  param
    AType = AN2 | ACN | AV | AV2 ;

  oper

    isTransitive : LinAtom -> Bool = \atom -> case atom.atype of {
      AN2|AV2 => True ;
      _ => False
    } ;

    LinAtom : Type = {
      n2 : N2 ;
      cn : CN ;
      v  : S.V ; -- SyntaxEng.V = intransitive verb
      v2 : V2 ;
      atype : AType
      } ;
    dummyAtom : LinAtom = let dummyN2 : N2 = P.mkN2 (P.mkN "dummy") in {
      cn = mkCN dummyN2 ;
      v  = P.mkV "dummy" ;
      v2 = P.mkV2 "dummy" ;
      n2 = dummyN2 ;
      atype = ACN
      } ;

    mkAtom = overload {
      mkAtom : CN  -> LinAtom = \cn -> dummyAtom ** {cn = cn ; atype = ACN} ;
      mkAtom : N   -> LinAtom = \n  -> dummyAtom ** {cn = mkCN n ; atype = ACN} ;
      mkAtom : S.V -> LinAtom = \v  -> dummyAtom ** {v  = v ; atype = AV} ;
      mkAtom : V2  -> LinAtom = \v2 -> dummyAtom ** {v2 = v2 ; atype = AV2} ;
      mkAtom : N2  -> LinAtom = \n2 -> dummyAtom ** {n2 = n2 ; atype = AN2} ;
    } ;


  param

    -- Predicates

    Arity = Ar0 | Ar1 | Ar2  ;

  oper
    LinPred : Type = {atom : LinAtom ; arg : NP ; arity : Arity} ;

    mkPred : LinPred -> VPS = \pred ->
      myVPS (case pred.arity of {
            Ar2 => pred2 pred.atom pred.arg ;
            _ => pred1 pred.atom }) ; -- TODO: arity 0 ??
    pred1 : LinAtom -> VP = \atom -> case atom.atype of {
      AV  => mkVP atom.v ;
      ACN => mkVP atom.cn ;
      AN2 => mkVP (mkCN atom.n2) ;
      AV2 => mkVP atom.v2 something_NP } ;

    pred2 : LinAtom -> NP -> VP = \atom,obj ->
      let objAdv = mkAdv possess_Prep obj ;
       in case atom.atype of {
            AV  => mkVP (mkVP atom.v) objAdv ;
            ACN => mkVP (mkVP atom.cn) objAdv ;
            AN2 => mkVP (mkCN atom.n2 obj) ;
            AV2 => mkVP atom.v2  obj } ; -- throws rock

    myVPS : VP -> VPS = \vp -> MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
}
