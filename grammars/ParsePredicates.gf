abstract ParsePredicates = Predicates, ReducedWordNet - [in_N, in_A] ** {

flags
  startcat = FullPredicate ;
  cat
    FullPredicate ;
  fun

    -- Coercions to startcat
    p1 : Predicate1 -> Predicate ;
    p2 : Predicate2 -> Predicate ;
    gp : GenPredicate -> Predicate ;

    -- Map to ComplNP(2)
    PredNP : Polarity -> NP -> FullPredicate ; -- Owner, LegalOwner
    PredNP2 : Polarity -> NP -> Prep -> FullPredicate ; -- OwnerOf (argument)

    -- Map to ComplAP(2)
    PredAP : Polarity -> AP -> FullPredicate ; -- Legal, AuthorizedToPracticeLaw
    PredAP2 : Polarity -> AP -> Prep -> FullPredicate ; -- AuthorizedToPracticeLawIn (argument)

    -- Map to ComplVPSlash "Described in Section 1"
    V2PartAdv : Polarity -> V2 -> Adv -> FullPredicate ;

    -- Map to GenPredicate
    PredSentence : NP -> VPS -> FullPredicate ; -- JurisdictionIsSingapore
    PredSentence2 : NP -> VPS2 -> FullPredicate ; -- Rule 34 applies

    -- Internal error messages
    PartialParseAfterNTokens,
    ParseFailedAfterNTokens : Int -> FullPredicate ;
    NoParse : FullPredicate ;

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

    FullPred : Predicate -> FullPredicate ;
  -- FullPred : Agreement -> Polarity -> Predicate -> Utt ;

}
