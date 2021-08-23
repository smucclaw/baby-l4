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
      v  : PolVPS ; -- intransitive verb
      v2 : PolVPS2 ;
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
      mkAtom : (pos, neg : VPS) -> LinAtom = \pos,neg -> dummyAtom ** {
        v  = table {MyPos => pos ; MyNeg => neg} ;
        atype = AV} ;
      mkAtom : (pos, neg : VPS2) -> LinAtom = \pos,neg -> dummyAtom ** {
        v2 = table {MyPos => pos ; MyNeg => neg} ;
        atype = AV2} ;
    } ;

  oper
    PolVPS : Type = MyPol => VPS ;
    PolVPS2 : Type = MyPol => VPS2 ;

    LinPred : Type = {atom : LinAtom ; arg : NP } ;

    mkPred : LinPred -> PolVPS = \pred ->
      case isTransitive pred.atom of {
        True => pred2 pred.atom pred.arg ;
        _    => pred1 pred.atom } ; -- TODO: arity 0 ??

    pred1 : LinAtom -> PolVPS = \atom -> case atom.atype of {
      AV  => atom.v ;
      ACN => myVPS (mkVP atom.cn) ;
      AN2 => myVPS (mkVP (mkCN atom.n2)) ;
      AV2 => \\pol => ComplVPS2 (atom.v2 ! pol) something_NP } ; -- or <atom.v2 : VPS>

    pred2 : LinAtom -> NP -> PolVPS = \atom,obj ->
      let vps2 : PolVPS2 = case atom.atype of {
            AV  => addC2 atom.v ;
            ACN => addC2 (myVPS (mkVP atom.cn)) ;
            AN2 => myVPS2 (N2VPSlash atom.n2) ;
            AV2 => atom.v2 } ; -- throws rock
        in \\pol => ComplVPS2 (vps2 ! pol) obj ;

    v2vps : S.V -> PolVPS = \v -> myVPS (mkVP v) ;
    v2vps2 : V2 -> PolVPS2 = \v2 -> myVPS2 (mkVPSlash v2) ;

--    myVPS : VP -> VPS = \vp -> MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
--    myVPS2 : VPSlash -> VPS2 = \v2 -> MkVPS2 (mkTemp presentTense simultaneousAnt) positivePol v2 ;

    myVPS : VP -> MyPol => VPS = \vp ->
      let vps : Pol -> VPS = \pol -> ExtendEng.MkVPS (mkTemp presentTense simultaneousAnt) pol vp ;
       in table { MyPos => vps positivePol ;
                  MyNeg => vps negativePol } ;
    myVPS2 : VPSlash -> MyPol => VPS2 = \vp ->
      let vps2 : Pol -> VPS2 = \pol -> ExtendEng.MkVPS2 (mkTemp presentTense simultaneousAnt) pol vp ;
       in table { MyPos => vps2 positivePol ;
                  MyNeg => vps2 negativePol } ;

    addC2 : PolVPS -> PolVPS2 = \pvps ->
      \\p => let vps : VPS = pvps ! p
              in lin VPS2 (vps ** {c2 = ""}) ;
  param
    MyPol = MyPos | MyNeg ;
}
