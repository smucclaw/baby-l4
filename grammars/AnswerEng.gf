concrete AnswerEng of Answer = AtomsEng ** open
   SyntaxEng, (C=ConjunctionEng), SymbolicEng, ExtendEng, Prelude in {
  lincat
    Statement = S ;
    [Statement] = BulletsOrInline => ListS ;
    Var, Arg = NP ;
    [Arg] = ListNP ;

  lincat
    Pred = LinPred ;
    [Pred] = {
      cn1 : CN ;
      cns : C.ListCN ;
      vp1 : VPS ;
      vps : [VPS] ;
      contentFields  : ContentFields ;
    } ;

  param
    ContentFields = VPS_CN1 | CNS_VP1 | CN1_VP1 | ONLY_CNS | ONLY_VPS | NoSingle  ;
    -- VPS_CN1 : for case where existing list has only 1 CN
    -- CNS_VP1 : for case where existing list has only 1 VPS
    -- NoSingle : eventual list would have lists with no hanging (single) cn1 nor vp1


  lin
   BasePred p1 p2 =
     case <p1.atom.atype, p2.atom.atype> of {
     <ACN|AN2, ACN|AN2> => dummyListPred ** {
        cns = C.BaseCN (toCN p1) (toCN p2) ;
        contentFields = ONLY_CNS };


     <AV|AV2, AV|AV2> => dummyListPred ** {
        vps = BaseVPS (mkPred p1) (mkPred p2) ;
        contentFields = ONLY_VPS ; };

     <ACN|AN2, AV| AV2> => dummyListPred ** {
       cn1 = toCN p1 ;
       vp1 = mkPred p2 ;
       contentFields = CN1_VP1};

    <AV|AV2, ACN| AN2> => dummyListPred ** {
       vp1 = mkPred p1 ;
       cn1 = toCN p2 ;
       contentFields = CN1_VP1 }


     };

  ConsPred p ps = case <p.atom.atype, ps.contentFields> of {
    -- p.atom.atype tells the type of the atom p ie vp or noun or cn2 vp1
    -- ps.contentFields tells which param value of the contentfields for ps
    <ACN|AN2 , VPS_CN1>      => ps ** {
      cns = C.BaseCN (toCN p) ps.cn1 ;
      contentFields = NoSingle
      };
    <ACN|AN2 , CNS_VP1>      => ps ** {
      cn1 = toCN p} ;

    <ACN|AN2 , CN1_VP1>  => ps ** {
      cns = C.BaseCN (toCN p) ps.cn1 ;
      contentFields =
      };

    <ACN|AN2 , ONLY_CNS> |  <ACN|AN2 , NoSingle> |  <ACN|AN2 , CNS_VP1>
      => ps ** {cns = C.ConsCN (toCN p) ps.cns} ;

    <ACN|AN2 , ONLY_VPS> =>

    <AV|AV2  , VPS_CN1>      =>
    <AV|AV2 , CNS_VP1>       =>
    <AV|AV2 , CN1_VP1>   =>
    <AV|AV2 , ONLY_CNS>       =>
    <AV|AV2 , ONLY_VPS>  | <AV|AV2 , NoSingle>
     => ps ** {vps = ConsVPS (mkPred p) ps.vps} ;

  }

    -- Coercions to Arg
    AVar v = v ;
    AAtom atom = case atom.atype of {
      ACN => mkNP atom.cn ;
      _ => nothing_NP --Predef.error "AAtom: trying to make predicate into argument"
    } ;

    -- Atom -> Arg -> Statement ;
    App1 atom arg = PredVPS arg (pred1 atom) ;

    -- : Atom -> (x, y : Arg) -> Statement ;
    App2 atom subj obj = PredVPS subj (pred2 atom obj) ;

    -- : String -> Var
    V = symb ;

    -- Aggregation functions

    -- : Atom -> Arg -> [Arg] -> Statement ; -- A and B are participants in C
    AggregateSubj1 isplayer players =
      App1 isplayer (mkNP and_Conj players) ;

    -- : Atom -> Arg -> [Arg] -> Statement ; -- A and B are participants in C
    AggregateSubj2 throws rock players =
      App2 throws (mkNP and_Conj players) rock ;

    -- Aggregations
    TransPred atom arg = {atom = atom ; arg = arg} ;
    IntransPred atom = {atom = atom ; arg = nothing_NP} ;

    AggregatePred2 p1 p2 subjs =
      let subj : NP = mkNP and_Conj subjs ;
          vps1 : VPS = mkPred p1 ;
          vps2 : VPS = mkPred p2 ;
          cnConjS : CN -> CN -> S = \cn1,cn2 -> mkS (mkCl subj (C.ConjCN and_Conj (C.BaseCN cn1 cn2))) ;
       in case <p1.atom.atype, p2.atom.atype> of {
            <ACN, ACN> => cnConjS p1.atom.cn p2.atom.cn ;
            <ACN, AN2> =>
              let comp : CN = mkCN p2.atom.n2 p2.arg ;
               in cnConjS p1.atom.cn comp ;
            <AN2, ACN> =>
              let comp : CN = mkCN p1.atom.n2 p1.arg ;
               in cnConjS p2.atom.cn comp ;
            _ => PredVPS subj (ConjVPS and_Conj (BaseVPS vps1 vps2))
          } ;

    AggregatePred3 p1 p2 p3 subjs =
      let subj : NP = mkNP and_Conj subjs ;
          vps1 : VPS = mkPred p1 ;
          vps2 : VPS = mkPred p2 ;
          vps3 : VPS = mkPred p3 ;
          toS  : CN -> S = \cn -> mkS (mkCl subj cn) ;
       in case <p1.atom.atype, p2.atom.atype, p3.atom.atype> of {
            <ACN, AN2, AN2> => toS (twoN2s p1 p2 p3) ;
            <AN2, ACN, AN2> => toS (twoN2s p2 p1 p3) ;
            <AN2, AN2, ACN> => toS (twoN2s p3 p1 p2) ;
            <ACN, ACN, AN2> => toS (oneN2 p1 p2 p3) ;
            <ACN, AN2, ACN> => toS (oneN2 p1 p3 p2) ;
            <AN2, ACN, ACN> => toS (oneN2 p2 p3 p1) ;
            <AN2, AN2, AN2> => toS (allN2s p1 p2 p3) ;
            _ => PredVPS subj (ConjVPS and_Conj (ConsVPS vps1 (BaseVPS vps2 vps3)))
          } ;
  oper
    -- Helper to make AggregatePred3 less redundant
    allN2s : (p,q,r : LinPred) -> CN = \p,q,r ->
        let comp1 : CN = mkCN p.atom.n2 p.arg ;
            comp2 : CN = mkCN q.atom.n2 q.arg ;
            comp3 : CN = mkCN r.atom.n2 r.arg ;
         in cnConj comp1 comp2 comp3 ;
    twoN2s : (p,q,r : LinPred) -> CN = \p,q,r ->
        let comp1 : CN = mkCN p.atom.cn ;
            comp2 : CN = mkCN q.atom.n2 q.arg ;
            comp3 : CN = mkCN r.atom.n2 r.arg ;
         in cnConj comp1 comp2 comp3 ;
    oneN2 : (p,q,r : LinPred) -> CN = \p,q,r ->
        let comp1 : CN = mkCN p.atom.cn ;
            comp2 : CN = mkCN q.atom.cn ;
            comp3 : CN = mkCN r.atom.n2 r.arg ;
         in cnConj comp1 comp2 comp3 ;

    cnConj : CN -> CN -> CN -> CN = \cn1,cn2,cn3 ->
      C.ConjCN and_Conj (C.ConsCN cn1 (C.BaseCN cn2 cn3)) ;

    -- empty list case
   dummyListPred = {
      cn1 : CN = dummyAtom.cn ;
      cns : C.ListCN = C.BaseCN dummyAtom.cn dummyAtom.cn ;
      vp1 : VPS = dummyAtom.v ;
      vps : [VPS] = ExtendEng.BaseVPS dummyAtom.v dummyAtom.v ;
      contentFields = NoSingle ;

   };

   toCN : LinPred -> CN ;
   toCN p1 = case p1.atom.atype of {
     ACN => p1.atom.cn ;
     AN2 => mkCN p1.atom.n2 p1.arg ;
     _   => dummyAtom.cn
     };

  lin

    -- : Statement -> Statement -> Statement ; -- A wins B if …
    IfThen = mkS if_Conj ;

    -- : Statement -> Statement -> [Statement]
    BaseStatement s1 s2 = table {
      TInline => mkListS s1 s2 ;
      TBullets => mkListS (addBullet s1) (addBullet s2)
      } ;

    -- : Statement -> [Statement] -> [Statement]
    ConsStatement s ss = table {
      TInline => mkListS s (ss ! TInline) ;
      TBullets => mkListS (addBullet s) (ss ! TBullets)
      } ;

    -- : Typography -> [Statement] -> Statement ;
    ConjStatement t ss = mkS and_Conj (ss ! t.t) ;
    DisjStatement t ss = mkS or_Conj (ss ! t.t) ;

    BaseArg = mkListNP ;
    ConsArg = mkListNP ;

    -- : Typography
  lincat
    Typography = {t : BulletsOrInline} ;
  lin
    Inline = {t = TInline} ;
    Bullets = {t = TBullets} ;

  oper
    if_Conj = and_Conj ** {s2 = "if"} ;

    addBullet : S -> S = \s -> s ** {s = "\\*" ++ s.s} ;

  param
    BulletsOrInline = TBullets | TInline ;

}
