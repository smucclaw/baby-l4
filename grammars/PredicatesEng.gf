
concrete PredicatesEng of Predicates =
  NounEng - [PPartNP, UseN2, RelNP, DetNP, AdvNP, PossNP, PartNP, CountNP, AdvCN, ApposCN,
             IndefArt, MassNP], -- Want to parse NPs without article mainly as CNs if possible
  VerbEng - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula, AdvVP, AdvVPSlash, VPSlashPrep],
  AdjectiveEng - [ReflA2, CAdvAP, UseA2, AdvAP],
  AdverbEng - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceEng - [UseCl],
  QuestionEng,
  RelativeEng - [IdRP],
  ConjunctionEng,
  PhraseEng - [UttAP, UttVP],
  IdiomEng,
  NumeralEng,
  ExtendEng [
    GerundCN,PresPartAP,PastPartAP,PastPartAgentAP,CompoundN,PositAdVAdj,
    VPS, ListVPS, ConjVPS2, BaseVPS, ConsVPS,
    VPS2, ListVPS2,ConjVPS2, BaseVPS, ConsVPS,
    VPI, ListVPI, ConjVPI2, BaseVPI, ConsVPI,
    VPI2, ListVPI2,ConjVPI2, BaseVPI, ConsVPI,
    MkVPS, MkVPS2, MkVPI, MkVPI2, PredVPS, A2VPSlash, N2VPSlash],
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  ConstructionEng,
  AtomsEng
   ** open ResEng, ExtraEng, (VE=VerbEng), (SE=SentenceEng), (P=ParadigmsEng), ExtendEng, SyntaxEng, SymbolicEng, Prelude in {

flags
  case_sensitive = off;

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;

  IdRP = which_who_RP ;

  lincat
--    Predicate = MyAgr => S ;
    Predicate1, GenPredicate = LinGenPred ;
    Predicate2 = LinGenPred2 ;

  lin
    CnNum cn card = mkCN cn (mkNP (mkDet card)) ;
    Int2Card int =  symb (mkSymb int.s) ; -- mkSymb : Str -> Symb ;
    CnInt cn int = mkCN cn (symb int) ;
    partyX x = mkCN (P.mkN "party") (symb x) ;
    thereby_AdV = lin AdV (P.mkAdv "thereby") ;
    hereby_AdV = lin AdV (P.mkAdv "hereby") ;
    henceforth_AdV = lin AdV (P.mkAdv "henceforth") ;

    CompoundA n ap = mkAP (lin AdA (mkUtt n)) ap ;

    APInf ap vp = SentAP ap (SE.EmbedVP vp) ;

    MkN2 n prep = P.mkN2 n prep ;
    MkCN2 cn prep = P.mkN2 <cn : N> prep ; -- CN and N have the same lincat, can do this
    MkV2 v prep = P.mkV2 v prep ;
    MkA2 a prep = P.mkA2 a prep ;

    NegAP ap = mkAP (lin AdA {s = "non"}) ap ;

    Must vp = VE.SlashVV must_VV vp ;
    May vp = VE.SlashVV may_VV vp ;
    Shall vp = VE.SlashVV shall_VV vp ;

  -- oper
  --   vvPred : VV -> VPSlash -> LinPred = \vv,vps -> \\agr =>
  --     mkCl (dummyNP ! agr) (ComplVV vv (mkVP vps (dummyNP ! agr))) ;

  lin
    --  : Predicate1 -> VPI2 -> Predicate2 ; -- amounts to a waiver of rights to enforce
    AddTransInf pred1 vpi2 = pred1 ** {
      pred = lin VPS2 (
        {s = \\o,a => let vp = pred1.pred.s ! o ! a in
                        {fin = vp.fin ;
                         inf = vp.inf ++ vpi2.s ! VVInf ! a } ;
        c2 = vpi2.c2})
    } ;

    AddPreposition1 pred1 prep = pred1 ** {
      pred = pred1.pred ** {c2 = prep.s}
    } ;

    AddPreposition2 pred2 prep = pred2 ** {
      pred = pred2.pred ** {c2 = pred2.pred.c2 ++ prep.s}
    } ;

    ComplV2V t p v2v np = headlessVP (MkVPS t p (mkVP <v2v : V2> np)) ;

    ComplNP np = headlessVP np ;
    ComplNP2 np prep = headlessVP2 np prep ;

    -- : AP -> Predicate1 ; -- Legal, AuthorizedToPracticeLaw
    ComplAP ap = headlessVP ap ;

    -- : AP -> Prep -> Predicate2 ; -- AuthorizedToPracticeLawIn (argument)
    ComplAP2 ap prep = headlessVP2 ap prep ;
--    ComplAdv : Adverbial -> Predicate ; --

    -- : VP -> Predicate ; --
    ComplVP vp = headlessVP vp ;

    -- : VP -> Predicate ; --
    ComplVP2 vp prep = headlessVP2 vp prep ;

    ComplVPSlash1 vpslash = headlessVP <vpslash : VPS> ;
    ComplVPSlash2 vpslash = headlessVP2 vpslash ;

    -- ComplVPSlash : VPSlash -> Predicate ;

    -- : NP -> VPS -> Predicate ; -- JurisdictionIsSingapore
    ComplSentence np vp = {subj = \\_ => np ; pred = vp} ;

  param
    MyAgr = MySg | MyPl ;
  oper
    LinGenPred2 : Type = {
      subj : MyAgr => NP ; -- dummyNP for all but GenPredicate
      pred : VPS2
    } ;
    LinGenPred : Type = {
      subj : MyAgr => NP ; -- dummyNP for all but GenPredicate
      pred : VPS
      } ;

    headlessVP = overload {
      headlessVP : AP -> LinGenPred = \np -> {subj = dummyNP ; pred = myVPS (mkVP np)} ;
      headlessVP : NP -> LinGenPred = \np -> {subj = dummyNP ; pred = myVPS (mkVP np)} ;
      headlessVP : VPS -> LinGenPred = \vp -> {subj = dummyNP ; pred = vp}
      } ;
    headlessVP2 = overload {
      headlessVP2 : NP -> Prep -> LinGenPred2 = \np,prep -> {subj = dummyNP ; pred = vpSlash np prep} ;
      headlessVP2 : AP -> Prep -> LinGenPred2 = \np,prep -> {subj = dummyNP ; pred = vpSlash np prep} ;
      headlessVP2 : VPS -> Prep -> LinGenPred2 = \vp,prep -> {subj = dummyNP ; pred = vpSlash vp prep} ;
      headlessVP2 : VPS2 -> LinGenPred2 = \vps2 -> {subj = dummyNP ; pred = vps2}
    } ;
    dummyNP : MyAgr => NP = table {
      MySg => it_NP ** {s = \\_ => []} ;
      MyPl => they_NP ** {s = \\_ => []}
      } ;

    vpSlash = overload {
      vpSlash : NP -> Prep -> VPS2 = \np,prep -> myVPS2 (VE.VPSlashPrep (mkVP np) prep) ;
      vpSlash : AP -> Prep -> VPS2 = \ap,prep -> myVPS2 (VE.VPSlashPrep (mkVP ap) prep) ;
      vpSlash : VPS -> Prep -> VPS2 = \vps,prep -> vps ** {c2 = prep.s} ;
      } ;

}
