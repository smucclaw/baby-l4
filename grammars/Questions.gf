abstract Questions = Atoms, Predicates - [p1, p2] ** {

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
    -- These are for the case where there is no user lexicon, and we just guess using smart paradigms.
    MkPred0 : (name : Atom)                    -> Pred ;
    MkPred1 : (name : Atom) -> (a1     : Atom) -> Pred ;
    MkPred2 : (name : Atom) -> (a1, a2 : Atom) -> Pred ;

    -- Make preds out of parse results.
    p1 : Predicate1 -> Atom ;
    p2 : Predicate2 -> Atom ;


}
