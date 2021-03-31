abstract Questions = {

  flags startcat = Question ;

  cat
    Question ;

    Atom ;
    Name ;
    Pred ;

  fun
    AreThereAny,
    AreThereMore
      : Pred -> Question ;

    Properties : Pred -> Question ;

    MkPred1 : (name : Name) -> (a1     : Atom) -> Pred ;
    MkPred2 : (name : Name) -> (a1, a2 : Atom) -> Pred ;

    dummyName : Name ;
    dummyAtom : Atom ;
}
