abstract Questions = Atoms ** {

  flags startcat = Question ;

  cat
    Question ;
    Pred ; -- different Pred than in Answer.gf

  fun
    AreThereAny,
    AreThereMore
      : Pred -> Question ;

    Properties : Pred -> Question ;

    MkPred0 : (name : Name)                    -> Pred ;
    MkPred1 : (name : Name) -> (a1     : Atom) -> Pred ;
    MkPred2 : (name : Name) -> (a1, a2 : Atom) -> Pred ;

    -- Test lexicon
    Win : Name ;
    Rock : Name ;
    Winner : Name ;
    Player : Atom ;
    Game : Atom ;


}
