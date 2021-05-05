abstract ParsePredicates = Predicates, ReducedWordNet - [in_N, in_A] ** {

flags
  startcat = Predicate ;
  cat
    Predicate ;

  fun
    -- Coercions to startcat
    p0 : NP -> Predicate ; -- Owner, LegalOwner
    p1 : Predicate1 -> Predicate ;
    p2 : Predicate2 -> Predicate ;
    gp : GenPredicate -> Predicate ;

    -- Map to ComplNP(2)
    PredNP2 : Polarity -> NP -> Prep -> Predicate ; -- OwnerOf (argument)

    -- Map to ComplAP(2)
    PredAP : Polarity -> AP -> Predicate ; -- Legal, AuthorizedToPracticeLaw
    PredAP2 : Polarity -> AP -> Prep -> Predicate ; -- AuthorizedToPracticeLawIn (argument)

    -- Map to ComplVPSlash "Described in Section 1"
    V2PartAdv : Polarity -> V2 -> Adv -> Predicate ;

    -- Map to GenPredicate
    PredSentence : NP -> VPS -> Predicate ; -- JurisdictionIsSingapore
    PredSentence2 : NP -> VPS2 -> Predicate ; -- Rule 34 applies

    -- Internal error messages
    PartialParseAfterNTokens,
    ParseFailedAfterNTokens : Int -> Predicate ;
    NoParse : Predicate ;

  cat
    Agreement ;
    Polarity ;

  fun
    SgAgr, PlAgr : Agreement ; -- don't need persons here. TODO: revisit if need reflexive?

    PosPol, NegPol : Polarity ; -- ???

    -- PresIndSg, PresIndPl,
    --   --PastInd,
    --   PPartInd,
    --   Gerund, Imperative : AgrTAM ; -- TODO see if need e.g. conditional

    --FullPred : Predicate -> FullPredicate ;
  -- FullPred : Agreement -> Polarity -> Predicate -> Utt ;

}
