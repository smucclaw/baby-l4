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
    ContentFields =
      VPS_CN1  -- the list has at least 2 VPSs, but only one CN
    | CNS_VP1  -- the list has at least 2 CNSs, but only one VPS
    | CN1_VP1  -- the list has one CN and one VPS
    | ONLY_CNS  -- the list has only CNs, no VPSs
    | ONLY_VPS  -- the list has only VPSs, no CNs
    | NoSingle  -- no hanging (single) cn1 nor vp1 (so empty, or complete)
    ;

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
       contentFields = CN1_VP1 }};

  ConsPred p ps = case <p.atom.atype, ps.contentFields> of {
    -- p.atom.atype tells the type of the atom p ie vp or noun or cn2 vp1
    -- ps.contentFields tells which param value of the contentfields for ps
    <ACN|AN2, CN1_VP1>  => ps ** {   -- can only come from BasePred: only two preds have been added to the list
      cns = C.BaseCN (toCN p) ps.cn1 ;
      contentFields = CNS_VP1 } ;

    <ACN|AN2, ONLY_VPS> => ps ** { -- can be from BasePred with two VPSs, and arbitrary applications of ConsPred with only VPSs.
      cn1 = toCN p ;
      contentFields = VPS_CN1 } ;

    <ACN|AN2, VPS_CN1> => ps ** {  -- at least 2 VPSs already, and one loose hanging CN. Must have been at least one ConsPred, possibly more
      cns = C.BaseCN (toCN p) ps.cn1 ;
      contentFields = NoSingle } ;

    -- Special thing with these three:
    -- * The cns field already contains a list, so we just call ConsCN
    -- * We don't need to change contentFields: the CNS part doesn't change, because it was already a list, and neither does VPS (because we didn't have a VPS).
    <ACN|AN2, ONLY_CNS> | <ACN|AN2, NoSingle> |  <ACN|AN2, CNS_VP1>
      => ps ** {cns = C.ConsCN (toCN p) ps.cns} ;

    <AV|AV2, CN1_VP1> => ps ** {
      vps = BaseVPS (mkPred p) ps.vp1 ;
      contentFields = VPS_CN1 } ;
    <AV|AV2, CNS_VP1> => ps ** {
      vps = BaseVPS (mkPred p) ps.vp1 ;
      contentFields = NoSingle } ; -- now it's complete
    <AV|AV2, ONLY_CNS> => ps ** {
      vp1 = mkPred p ;
      contentFields = CNS_VP1 } ;
    <AV|AV2, ONLY_VPS> | <AV|AV2, NoSingle> | <AV|AV2, VPS_CN1>
      => ps ** {vps = ConsVPS (mkPred p) ps.vps}
  } ;
  oper
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

    -- : [Pred] -> [Arg] -> Statement ;
    AggregatePred preds subjs =
      let
        subj : NP = mkNP and_Conj subjs ;
        cn2vps : CN -> VPS = \cn -> myVPS (mkVP cn) ;
        -- the subject is [a human, a cat and a dog]
        cn2s : C.ListCN -> S = \cns -> mkS (mkCl subj (C.ConjCN and_Conj cns)) ;

        -- the subject [throws rock, beats scissors]
        vp2s : [VPS] -> S = \vps -> PredVPS subj (ConjVPS and_Conj vps) ; -- PredVPS comes from Extend, type signature NP -> VPS -> S
      in case preds.contentFields of {
        ONLY_CNS => cn2s preds.cns ;
        ONLY_VPS => vp2s preds.vps ;
        NoSingle => let cnVPS : VPS = cn2vps (C.ConjCN and_Conj preds.cns) ; -- is a cat and a player
                        allVPS : [VPS] = ConsVPS cnVPS preds.vps -- is a cat and a player, throws rock and beats scissors
                     in vp2s allVPS ;
        VPS_CN1 => let cnVPS : VPS = cn2vps preds.cn1 ; -- is a cat
                        allVPS : [VPS] = ConsVPS cnVPS preds.vps -- is a cat, throws rock and beats scissors
                     in vp2s allVPS ;
        CNS_VP1 => let cnVPS : VPS = cn2vps (C.ConjCN and_Conj preds.cns) ; -- is a cat and a player
                        allVPS : [VPS] = BaseVPS cnVPS preds.vp1 -- is a cat and a player, and throws rock
                     in vp2s allVPS ;  -- TODO: enforce oxford comma
        CN1_VP1 => let cnVPS : VPS = cn2vps preds.cn1 ;           -- is a cat
                       allVPS : [VPS] = BaseVPS cnVPS preds.vp1  -- is a cat and throws rock
                    in vp2s allVPS
      } ;


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
