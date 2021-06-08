concrete AtomsEng of Atoms = open Prelude, SyntaxEng, ExtendEng, (S=SyntaxEng), (P=ParadigmsEng) in {
  lincat
    Atom = LinAtom ;
    --Pred = LinPred ;

  param
    AType = AN2 | AV2 -- Arity = 2
          | ACN | AV  -- Arity = 1
          ;
  oper

    isTransitive : LinAtom -> Bool = \atom -> case atom.atype of {
      AN2|AV2 => True ;
      _ => False
    } ;

    LinAtom : Type = {
      n2 : N2 ;
      cn : CN ;
      v  : VPS ; -- intransitive verb
      v2 : VPS2 ;
      atype : AType
      } ;

    dummyAtom : LinAtom = let dummyN2 : N2 = P.mkN2 (P.mkN "dummy") in {
      cn = mkCN dummyN2 ;
      v  = v2vps (P.mkV "dummy") ;
      v2 = v2vps2 (P.mkV2 "dummy") ;
      n2 = dummyN2 ;
      atype = ACN
      } ;

    mkAtom = overload {
      mkAtom : CN  -> LinAtom = \cn -> dummyAtom ** {cn = cn ; atype = ACN} ;
      mkAtom : N   -> LinAtom = \n  -> dummyAtom ** {cn = mkCN n ; atype = ACN} ;
      mkAtom : S.V -> LinAtom = \v  -> dummyAtom ** {v  = v2vps v ; atype = AV} ;
      mkAtom : V2  -> LinAtom = \v2 -> dummyAtom ** {v2 = v2vps2 v2 ; atype = AV2} ;
      mkAtom : N2  -> LinAtom = \n2 -> dummyAtom ** {n2 = n2 ; atype = AN2} ;
      mkAtom : VPS -> LinAtom = \v  -> dummyAtom ** {v  = v ; atype = AV} ;
      mkAtom : VPS2  -> LinAtom = \v2 -> dummyAtom ** {v2 = v2 ; atype = AV2} ;
    } ;

  oper
    LinPred : Type = {atom : LinAtom ; arg : NP } ;

    mkPred : LinPred -> VPS = \pred ->
      case isTransitive pred.atom of {
        True => pred2 pred.atom pred.arg ;
        _ => pred1 pred.atom } ; -- TODO: arity 0 ??

    pred1 : LinAtom -> VPS = \atom -> case atom.atype of {
      AV  => atom.v ;
      ACN => myVPS (mkVP atom.cn) ;
      AN2 => myVPS (mkVP (mkCN atom.n2)) ;
      AV2 => ComplVPS2 atom.v2 something_NP } ; -- or <atom.v2 : VPS>

    pred2 : LinAtom -> NP -> VPS = \atom,obj ->
      let addC2 : VPS -> VPS2 = \vps -> vps ** {c2 = ""} ;
          vps2 : VPS2 = case atom.atype of {
            AV  => addC2 atom.v ;
            ACN => addC2 (myVPS (mkVP atom.cn)) ;
            AN2 => myVPS2 (N2VPSlash atom.n2) ;
            AV2 => atom.v2 } ; -- throws rock
        in ComplVPS2 vps2 obj ;

    v2vps : S.V -> VPS = \v -> myVPS (mkVP v) ;
    v2vps2 : V2 -> VPS2 = \v2 -> myVPS2 (mkVPSlash v2) ;

    myVPS : VP -> VPS = \vp -> MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
    myVPS2 : VPSlash -> VPS2 = \v2 -> MkVPS2 (mkTemp presentTense simultaneousAnt) positivePol v2 ;
}
