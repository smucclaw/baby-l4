
concrete ParsePredicatesEng of ParsePredicates =
  ReducedWordNetEng - [in_N, in_A], 
  PredicatesEng ** open ResEng, ExtraEng, (V=VerbEng), (P=ParadigmsEng), ExtendEng, SyntaxEng, Prelude in {

flags
  case_sensitive = off;

  lincat
    Predicate = Utt ;
    Agreement = {a : MyAgr} ; -- Agreement is redundant in all but present
    Polarity = {p : Pol ; s : Str} ;

  oper
    anyAgr = MySg | MyPl ;
  lin

    p0 cn = mkUtt (mkNP cn) ; -- LegalOwner

    gp, p1 = \pred1 -> mkUtt (PredVPS (pred1.subj ! anyAgr) pred1.pred) ;
    p2 pred2 = mkUtt (PredVPS (pred2.subj ! anyAgr) (ComplVPS2 pred2.pred (dummyNP ! MySg))) ;

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

    PartialParseAfterNTokens n = lin Utt (cc3 (ss "partial parse after") n (ss "tokens")) ;
    ParseFailedAfterNTokens n = lin Utt (cc3 (ss "parse failed after") n (ss "tokens")) ;
    NoParse = lin Utt (ss "no parse") ;


  lin

    -- : CN -> Prep -> Predicate ; -- OwnerOf (argument)
    PredNP2 cn prep = 
      let np : NP = mkNP cn|mkNP aPl_Det cn 
       in cc2 (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;

    --  : Polarity -> AP -> Predicate ;
    PredAP pol np = cc2 pol (mkUtt np) ;
    -- : Polarity -> AP -> Prep -> Predicate ;
    PredAP2 pol np prep = cc3 pol (mkUtt np) (mkAdv prep (dummyNP ! MySg)) ;

    V2PartAdv pol v2 adv = PredAP pol (AdvAP (PastPartAP (mkVPSlash v2)) adv) ;

  -- Extensions for disambiguation questions

  -- : N -> N -> N ; -- Replace CompoundN with this to make explicit
  CompoundNHyphen noun cn = cn ** {
    s = \\n,c => noun.s ! Sg ! Nom ++ BIND++"-"++BIND ++ cn.s ! n ! c} ;

} ;
