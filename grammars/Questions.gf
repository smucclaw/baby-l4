abstract Questions = Atoms, Predicates ** {

  flags startcat = Question ;

  cat
    Question ;
    Pred ; -- different Pred than in Answer.gf

  fun
    AreThereAny,
    AreThereMore
      : Pred -> Question ;

    Properties : Pred -> Question ;

    -- Make preds out of atoms.
    -- The atoms may come from user lexicon, or they may be guessed using smart paradigms.
    MkPred0 : (name : Atom)                    -> Pred ;
    MkPred1 : (name : Atom) -> (a1     : Atom) -> Pred ;
    MkPred2 : (name : Atom) -> (a1, a2 : Atom) -> Pred ;
}
