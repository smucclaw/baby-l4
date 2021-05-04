-- NB. This grammar doesn't open a lexicon. It is used as a base for two grammars that do:
-- * ParsePredicates that uses ReducedWordNet, i.e. WN without sense numbers. Used for initial parsing + asking user to resolve ambiguities.
-- * TopPredicates that uses real WordNet, and that will be used in the actual application grammar.

abstract Predicates =
  Noun - [PPartNP, UseN2, RelNP, DetNP, AdvNP, PossNP, PartNP, CountNP, ApposCN],
  Verb - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, AdvVP],
  Adjective - [ReflA2, CAdvAP, UseA2], --AdvAP],
  Adverb - [AdnCAdv, ComparAdvAdj, ComparAdvAdjS],
  Sentence - [EmbedVP],
  Question,
  Relative,
  Conjunction,
  Phrase - [UttAP, UttVP],
  Idiom,
  Numeral,
  Tense,
  Extend [
    GerundCN,PresPartAP,PastPartAP,PastPartAgentAP,CompoundN,PositAdVAdj,
    VPS, VPS2, MkVPS, MkVPS2, PredVPS,  A2VPSlash, N2VPSlash],
  Construction,
  Atoms ** {

flags
  startcat = Predicate ;

  fun
    -- Application-specific extensions
 
    CnNum : CN -> Card -> CN ; -- Section 1
    CompoundA : N -> AP -> AP ; -- owner-driven
    NegAP : AP -> AP ; -- non-executive
--    AdvVVP : Adv -> VP -> VP ; -- allow Adv to be used like AdV: materially interferes
    May,Must,Shall : VPSlash -> VPSlash ; -- allow SlashVV only for deontics

    APInf : AP -> VP -> AP ; -- authorized to practice law

    -- Quick and dirty way to get rid of attachment ambiguity: all Advs attach to the closest word
    MkN2 : N -> Prep -> N2 ;
    MkV2 : V -> Prep -> V2 ;
    MkA2 : A -> Prep -> A2 ;

  cat
    Predicate ;
    Predicate1 ; Predicate2 ; GenPredicate ;
  fun
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

    ComplSentence : NP -> VPS -> GenPredicate ; -- (the company's) jurisdiction is Singapore

    -- Coercions to startcat
    p1 : Predicate1 -> Predicate ;
    p2 : Predicate2 -> Predicate ;

}
