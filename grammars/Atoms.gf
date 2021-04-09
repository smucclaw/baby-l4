abstract Atoms = {
  -- Common cats shared by Answer and Questions
  cat
    Atom ;
    Name ;
    Pred ;

  -- Need to have something of the type, when Haskell code of the abstract syntax is generated.
  -- Actual atoms and names are generated dynamically.
  fun
    dummyAtom : Atom ;
    dummyName : Name  ;

}
