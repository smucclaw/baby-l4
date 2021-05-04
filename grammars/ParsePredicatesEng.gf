
concrete ParsePredicatesEng of ParsePredicates =
  ReducedWordNetEng - [in_N, in_A], 
  PredicatesEng ** open ResEng, ExtraEng, (V=VerbEng), (P=ParadigmsEng), ExtendEng, SyntaxEng, Prelude in {

flags
  case_sensitive = off;

  lincat
    FullPredicate = Utt ;
    Agreement = {a : MyAgr} ; -- Agreement is redundant in all but present
    Polarity = {p : Pol ; s : Str} ;
  lin

    p1 pred1 = \\agr => PredVPS (pred1.subj ! agr) pred1.pred ;
    p2 pred2 = \\agr => PredVPS (pred2.subj ! agr) (ComplVPS2 pred2.pred (dummyNP ! MySg)) ;

    PosPol = {p = positivePol ; s = ""} ;
    NegPol = {p = negativePol | UncontractedNeg ; s = "not"} ; -- a hack

    SgAgr = {a = MySg} ;
    PlAgr = {a = MyPl} ;

    PresIndSg = {t = mkTemp presentTense simultaneousAnt ; a = MySg} ;
    PresIndPl = {t = mkTemp presentTense simultaneousAnt ; a = MyPl} ;
    PastInd = {t = mkTemp pastTense simultaneousAnt ; a = MySg} ;
    PPartInd = {t = mkTemp presentTense anteriorAnt ; a = MySg} ;

    PredSentence np vps = mkUtt (PredVPS np vps) ;
    PredSentence2 np vps = PredSentence np <vps : VPS> ;

--    FullPred agrtam pol pred = mkUtt (UseCl agrtam.t pol.p (pred ! agrtam.a)) ;
   FullPred pr = mkUtt (pr ! (MySg|MyPl)) ;

    PartialParseAfterNTokens n = lin Utt (cc3 (ss "partial parse after") n (ss "tokens")) ;
    ParseFailedAfterNTokens n = lin Utt (cc3 (ss "parse failed after") n (ss "tokens")) ;
    NoParse = lin Utt (ss "no parse") ;


  lin
    --  : NP -> FullPredicate ; -- Owner, LegalOwner
    PredNP pol np = cc2 pol (mkUtt np) ;
    -- : NP -> Prep -> FullPredicate ; -- OwnerOf (argument)
    PredNP2 pol np prep = cc3 pol (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;
    --  : AP -> FullPredicate ;
    PredAP pol np = cc2 pol (mkUtt np) ;
    -- : AP -> Prep -> FullPredicate ;
    PredAP2 pol np prep = cc3 pol (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;

    V2PartAdv pol v2 adv = PredAP pol (AdvAP (PastPartAP (mkVPSlash v2)) adv) ;

} ;
