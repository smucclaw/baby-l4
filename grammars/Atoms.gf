abstract Atoms = {
  -- Common cats shared by Answer and Questions
  cat
    Atom ;
    Pred ;

  -- Need to have something of the type, when Haskell code of the abstract syntax is generated.
  -- Actual atoms are generated dynamically.
  fun
    dummyAtom : Atom ;

}
