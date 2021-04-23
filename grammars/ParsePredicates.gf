abstract ParsePredicates = Predicates, ParseExtend [UttVPS], ReducedWordNet - [in_N, in_A] ** {

flags
  startcat = Utt ;

  fun

    -- Map to ComplNP(2)
    PredNP : Polarity -> NP -> Utt ; -- Owner, LegalOwner
    PredNP2 : Polarity -> NP -> Prep -> Utt ; -- OwnerOf (argument)

    -- Map to ComplAP(2)
    PredAP : Polarity -> AP -> Utt ; -- Legal, AuthorizedToPracticeLaw
    PredAP2 : Polarity -> AP -> Prep -> Utt ; -- AuthorizedToPracticeLawIn (argument)

    -- Map to ComplVPSlash "Described in Section 1"
    V2PartAdv : Polarity -> V2 -> Adv -> Utt ;

    -- Map to GenPredicate
    PredSentence : Polarity -> NP -> VP -> Utt ; -- JurisdictionIsSingapore

    -- Internal error messages
    -- PartialParseAfterNTokens,
    -- ParseFailedAfterNTokens : Int -> Utt ;
    -- NoParse : Utt ;

  cat
    AgrTAM ;
    Polarity ;

  fun
--    SgAgr, PlAgr : Agreement ; -- simplify: don't really need persons here. TODO: revisit if need reflexive?

    PosPol, NegPol : Polarity ;

    PresIndSg, PresIndPl,
      --PastInd,
      PPartInd,
      Gerund, Imperative : AgrTAM ; -- TODO see if need e.g. conditional

    FullPred : AgrTAM -> Polarity -> Predicate -> Utt ;

}
