
concrete PredicatesEng of Predicates =
  NounEng - [PPartNP, UseN2, RelNP, DetNP, AdvNP, PossNP, PartNP, CountNP, AdvCN, ApposCN],
  VerbEng - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula, AdvVP, AdvVPSlash, VPSlashPrep],
  AdjectiveEng - [ReflA2, CAdvAP, UseA2, AdvAP],
  AdverbEng - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceEng - [UseCl, EmbedVP],
  QuestionEng,
  RelativeEng - [IdRP],
  ConjunctionEng,
  PhraseEng - [UttAP, UttVP],
  IdiomEng,
  NumeralEng,
  ExtendEng [
    GerundCN,PresPartAP,PastPartAP,PastPartAgentAP,CompoundN,PositAdVAdj,
    VPS, VPS2, MkVPS, MkVPS2, PredVPS, A2VPSlash, N2VPSlash],
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  ConstructionEng,
  AtomsEng
   ** open ResEng, ExtraEng, (VE=VerbEng), (SE=SentenceEng), (P=ParadigmsEng), ExtendEng, SyntaxEng, Prelude in {

flags
  case_sensitive = off;

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;

  IdRP = which_who_RP ;

  lincat
    Predicate = MyAgr => S ;
    Predicate1, GenPredicate = LinGenPred ;
    Predicate2 = LinGenPred2 ;

  lin
    CnNum cn card = mkCN cn (mkNP (mkDet card)) ;
    CompoundA n ap = mkAP (lin AdA (mkUtt n)) ap ;

    APInf ap vp = SentAP ap (SE.EmbedVP vp) ;

    MkN2 n prep = P.mkN2 n prep ;
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

    p1 pred1 = \\agr => PredVPS (pred1.subj ! agr) pred1.pred ;
    p2 pred2 = \\agr => PredVPS (pred2.subj ! agr) (ComplVPS2 pred2.pred (dummyNP ! MySg)) ;


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

--    myVPS : VP -> VPS = \vp -> MkVPS (mkTemp presentTense simultaneousAnt) positivePol vp ;
--    myVPS2 : VPSlash -> VPS2 = \vp -> MkVPS2 (mkTemp presentTense simultaneousAnt) positivePol vp ;
}
