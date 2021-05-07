-- NB. This grammar doesn't open a lexicon. It is used as a base for two grammars that do:
-- * ParsePredicates that uses ReducedWordNet, i.e. WN without sense numbers. Used for initial parsing + asking user to resolve ambiguities.
-- * TopPredicates that uses real WordNet, and that will be used in the actual application grammar.

abstract Predicates =
  Noun - [PPartNP, UseN2, RelNP, DetNP, AdvNP, PossNP, PartNP, CountNP, ApposCN], --IndefArt, MassNP], -- stupidest idea ever
  Verb - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, AdvVP],
  Adjective - [ReflA2, CAdvAP, UseA2], --AdvAP],
  Adverb - [AdnCAdv, ComparAdvAdj, ComparAdvAdjS],
  Sentence,
  Question,
  Relative,
  Conjunction,
  Phrase - [UttAP, UttVP],
  Idiom,
  Numeral,
  Tense,
  Extend [
    PrepCN,GerundCN,PresPartAP,PastPartAP,PastPartAgentAP,CompoundN,PositAdVAdj,
    VPS, ListVPS, ConjVPS2, BaseVPS, ConsVPS,
    VPS2, ListVPS2,ConjVPS2, BaseVPS, ConsVPS,
    VPI, ListVPI, ConjVPI2, BaseVPI, ConsVPI,
    VPI2, ListVPI2,ConjVPI2, BaseVPI, ConsVPI,
    MkVPS, MkVPS2, MkVPI, MkVPI2, PredVPS,
    A2VPSlash, N2VPSlash],
  Construction,
  Atoms ** {

  fun
    -- Application-specific extensions

    CnNum : CN -> Card -> CN ; -- Section one/34
    Int2Card : Int -> Card ;

    partyX : String -> CN ;


    thereby_AdV : AdV ;
    hereby_AdV: AdV ;
    henceforth_AdV : AdV ;

    CompoundA : N -> AP -> AP ; -- owner-driven
    NegAP : AP -> AP ; -- non-executive
--    AdvVVP : Adv -> VP -> VP ; -- allow Adv to be used like AdV: materially interferes
    May,Must,Shall : VPSlash -> VPSlash ; -- allow SlashVV only for deontics

    APInf : AP -> VP -> AP ; -- authorized to practice law

    -- Quick and dirty way to get rid of attachment ambiguity: all Advs attach to the closest word
    MkN2 : N -> Prep -> N2 ;
    MkCN2 : CN -> Prep -> N2 ; -- TODO: get rid of this again, creates ambiguity
    MkV2 : V -> Prep -> V2 ;
    MkA2 : A -> Prep -> A2 ;

  cat
    Predicate1 ; Predicate2 ; GenPredicate ;
  fun

    AddTransInf : Predicate1 -> VPI2 -> Predicate2 ; -- amounts to a waiver of rights to enforce
    AddPreposition1 : Predicate1 -> Prep -> Predicate2 ;
    AddPreposition2 : Predicate2 -> Prep -> Predicate2 ; -- maybe Predicate3 later?

    ComplV2V : Temp -> Pol -> V2V -> NP -> Predicate1 ; -- EntitlesHolder


    ComplNP : NP -> Predicate1 ; -- IsOwner,
    ComplNP2 : NP -> Prep -> Predicate2 ; -- IsOwnerOf (argument)

    ComplAP : AP -> Predicate1 ; -- IsAuthorizedToPracticeLaw
    ComplAP2 : AP -> Prep -> Predicate2 ; -- IsAuthorizedToPracticeLawIn (argument)
    ComplAdv : Adv -> Predicate1 ; --

    ComplVP : VPS -> Predicate1 ; -- MateriallyInterferesWithPracticingAsLawyer
    ComplVP2 : VPS -> Prep -> Predicate2 ; -- HeldAsRepresentativeOf
    ComplVPSlash1 : VPS2 -> Predicate1 ; -- DescribedInSection1
    ComplVPSlash2 : VPS2 -> Predicate2 ; -- Approves

    -- TODO: make these into NP -> Predicate{n} -> GenPredicate
    ComplSentence : NP -> VPS -> GenPredicate ; -- (the company's) jurisdiction is Singapore
    ComplSentence2 : NP -> VPS2 -> GenPredicate ; -- rule 34 applies
}
