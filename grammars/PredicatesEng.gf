
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
    GerundCN,PresPartAP,PastPartAP,PastPartAgentAP,CompoundN,PositAdVAdj,VPI],
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  ConstructionEng
   ** open ResEng, ExtraEng, (V=VerbEng), (P=ParadigmsEng), ExtendEng, SyntaxEng, Prelude in {

flags
  case_sensitive = off;

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;

  IdRP = which_who_RP ;

  lincat
    Predicate = LinPred ;
    FullPredicate = Utt ;

    AgrTAM = {t : Temp ; a : MyAgr} ; -- Agreement is redundant in all but present
    Polarity = {p : Pol ; s : Str} ;
  param
    MyAgr = MySg | MyPl ;

  lin
    CnNum cn card = mkCN cn (mkNP (mkDet card)) ;
    V2PartAdv pol v2 adv = PredAP pol (AdvAP (PastPartAP (mkVPSlash v2)) adv) ;
    CompoundA n ap = mkAP (lin AdA (mkUtt n)) ap ;

    PosPol = {p = positivePol ; s = ""} ;
    NegPol = {p = negativePol | UncontractedNeg ; s = "not"} ; -- a hack

    PresIndSg = {t = mkTemp presentTense simultaneousAnt ; a = MySg} ;
    PresIndPl = {t = mkTemp presentTense simultaneousAnt ; a = MyPl} ;
    PastInd = {t = mkTemp pastTense simultaneousAnt ; a = MySg} ;
    PPartInd = {t = mkTemp presentTense anteriorAnt ; a = MySg} ;
    -- TODO
    -- Gerund = {t = Gerund} ;
    -- Imperative = {t = Imperative} ;

    FullPred agrtam pol pred = mkUtt (UseCl agrtam.t pol.p (pred ! agrtam.a)) ;

    MkN2 n prep = P.mkN2 n prep ;
    MkV2 v prep = P.mkV2 v prep ;
    MkA2 a prep = P.mkA2 a prep ;

    NegAP ap = mkAP (lin AdA {s = "non"}) ap ;

    -- Must pol vp = FullPred PresIndSg pol (vvPred must_VV vp) ;
    -- May pol vp = FullPred PresIndSg pol (vvPred may_VV vp) ;
    -- Shall pol vp = FullPred PresIndSg pol (vvPred shall_VV vp) ;

    Must vp = (vvPred must_VV vp) ;
    May vp = (vvPred may_VV vp) ;
    Shall vp = (vvPred shall_VV vp) ;


    PartialParseAfterNTokens n = lin Utt (cc3 (ss "partial parse after") n (ss "tokens")) ;
    ParseFailedAfterNTokens n = lin Utt (cc3 (ss "parse failed after") n (ss "tokens")) ;
    NoParse = lin Utt (ss "no parse") ;

  oper
    vvPred : VV -> VPSlash -> LinPred = \vv,vps -> \\agr =>
      mkCl (dummyNP ! agr) (ComplVV vv (mkVP vps (dummyNP ! agr))) ;

  lin
    --  : NP -> FullPredicate ; -- Owner, LegalOwner
    PredNP pol np = cc2 pol (mkUtt np) ;
    -- : NP -> Prep -> FullPredicate ; -- OwnerOf (argument)
    PredNP2 pol np prep = cc3 pol (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;
    --  : AP -> FullPredicate ;
    PredAP pol np = cc2 pol (mkUtt np) ;
    -- : AP -> Prep -> FullPredicate ;
    PredAP2 pol np prep = cc3 pol (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;



    ComplV2V v2v np = headlessVP (mkVP <v2v : V2> np) ;

    ComplNP np = headlessVP np ;
    ComplNP2 np prep = headlessVP np prep ;

    -- : AP -> Predicate ; -- Legal, AuthorizedToPracticeLaw
    ComplAP ap = headlessVP ap ;

    -- : AP -> Prep -> Predicate ; -- AuthorizedToPracticeLawIn (argument)
    ComplAP2 ap prep = headlessVP ap prep ;
--    ComplAdv : Adverbial -> Predicate ; --

    -- : VP -> Predicate ; --
    ComplVP vp = headlessVP vp ;

    -- : VP -> Predicate ; --
    ComplVP2 vp prep = headlessVP vp prep ;

    ComplVPSlash vpslash = headlessVP vpslash ;

    -- ComplVPSlash : VPSlash -> Predicate ;

    -- : NP -> VP -> Predicate ; -- JurisdictionIsSingapore
    ComplSentence pol np vp = FullPred PresIndSg pol (\\_ => mkCl np vp) ;

    AdvVVP = V.AdVVP ;

  oper
    LinPred : Type = MyAgr => Cl ;


    headlessVP = overload {
      headlessVP : AP -> LinPred = \np -> \\agr => mkCl (dummyNP ! agr) np ;
      headlessVP : NP -> LinPred = \np -> \\agr => mkCl (dummyNP ! agr) np ;
      headlessVP : NP -> Prep -> LinPred = \np,prep -> \\agr => mkCl (dummyNP ! agr) (mkVP (vpSlash np prep) (dummyNP ! agr)) ;
      headlessVP : AP -> Prep -> LinPred = \np,prep -> \\agr => mkCl (dummyNP ! agr) (mkVP (vpSlash np prep) (dummyNP ! agr)) ;
      headlessVP : VP -> Prep -> LinPred = \vp,prep -> \\agr => mkCl (dummyNP ! agr) (mkVP (vpSlash vp prep) (dummyNP ! agr)) ;
      headlessVP : VPSlash -> LinPred = \vps -> \\agr => mkCl (dummyNP ! agr) (mkVP vps (dummyNP ! agr)) ;
      headlessVP : VP -> LinPred = \vp -> \\agr => mkCl (dummyNP ! agr) vp

      } ;

    dummyNP : MyAgr => NP = table {
      MySg => it_NP ** {s = \\_ => []} ;
      MyPl => they_NP ** {s = \\_ => []}
      } ;

    vpSlash = overload {

      vpSlash : NP -> Prep -> VPSlash = \np,prep -> V.VPSlashPrep (mkVP np) prep ;
      vpSlash : AP -> Prep -> VPSlash = \ap,prep -> V.VPSlashPrep (mkVP ap) prep ;
      vpSlash : VP -> Prep -> VPSlash = V.VPSlashPrep
      } ;
} ;
