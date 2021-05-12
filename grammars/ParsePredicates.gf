abstract ParsePredicates = Predicates, ReducedWordNet - [in_N, in_A] ** {

flags
  startcat = Predicate ;
  cat
    Predicate ;

  fun
    -- Coercions to startcat
    p0 : CN -> Predicate ; -- Owner, LegalOwner
    mkAtom : N2 -> Predicate ; -- complete hack
    p1 : Predicate1 -> Predicate ;
    p2 : Predicate2 -> Predicate ;
    gp : GenPredicate -> Predicate ;

    -- Map to ComplNP(2)
    PredNP2 : CN -> Prep -> Predicate ; -- OwnerOf (argument)

    -- Map to ComplAP(2)
    PredAP : Polarity -> AP -> Predicate ; -- Legal, AuthorizedToPracticeLaw
    PredAP2 : Polarity -> AP -> Prep -> Predicate ; -- AuthorizedToPracticeLawIn (argument)

    -- Map to ComplVPSlash "Described in Section 1"
    V2PartAdv : Polarity -> V2 -> Adv -> Predicate ;

    -- Map to GenPredicate
    PredSentence : NP -> VPS -> Predicate ; -- JurisdictionIsSingapore
    PredSentence2 : NP -> VPS2 -> Predicate ; -- Rule 34 applies 

    -- Internal errormessages
    -- PartialParseAfterNTokens,
    -- ParseFailedAfterNTokens : Int -> Predicate ;
    -- NoParse : Predicate ;

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


  -- Extensions for disambiguation questions
  CompoundNHyphen : N -> N -> N ; -- Replace CompoundN with this to make explicit
}
