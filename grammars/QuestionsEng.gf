concrete QuestionsEng of Questions = {

  lincat
--    Question = QS ;

    Pred = LinPred ;

  lin

    AreThereAny p = case p.arity of {
      A1 => {s = "Are there any" ++ p.s ++ "s?"} ;
      A2 => {s = "Are there any" ++ p.arg1 ++ "s who"
               ++ p.s ++ "the" ++ p.arg2 ++ "?"}
      } ;

    AreThereMore p = case p.arity of {
      A1 => {s = "Are there more" ++ p.s ++ "s?"} ;
      A2 => {s = "Are there more" ++ p.arg1 ++ "s who"
               ++ p.s ++ "the" ++ p.arg2 ++ "?"}
      };

    Properties p = case p.arity of {
      A1 => {s = "What's the name of the" ++ p.s ++ "?"} ;
      A2 => {s = "What's the name of the" ++ p.arg1 ++ "? \\ What's the name of the" ++ p.arg2 ++ "?"}
      --A2 => {s = "What are the names of" ++ p.arg1 ++ "and"
      --         ++ p.arg2 ++ "that the" ++ p.arg1 ++ p.s ++ "?"}
      } ;

    Rock = mkAtom "rock" ;
    Paper = mkAtom "paper" ;
    Scissors = mkAtom "scissors" ;
    Throw = mkName "throw" ;
    Win = mkName "win" ;
    Participate_in = mkName "participate in" ;

    Player = mkAtom "player" ;
    Game = mkAtom "game" ;
    Sign = mkAtom "sign" ;
    Object = mkAtom "object" ;

    MkPred1 nm a1 = mkPred nm.s a1.s ;
    MkPred2 nm a1 a2 = mkPred nm.s a1.s a2.s ;

  param
    Arity = A1 | A2  ;

  oper
    -- To be changed later
    Arg : Type = Str ;
    mkArg : Str -> Arg = \s -> s ;

    -- These should work unchanged
    dummyArg : Arg = mkArg "dummy" ;

    mkName,mkAtom : Str -> {s : Str} = \s -> {s=s} ;

    LinPred : Type = {
      s : Str ;
      arg1 : Arg ;
      arg2 : Arg ;
      arity : Arity ;
      };

    mkPred = overload {
      mkPred : Str -> (a1 : Arg) -> LinPred = \nm,a1 -> {
         s = nm ;
         arg1 = a1 ;
         arg2 = dummyArg ;
    Name, Arg = Str ;
     Name, Arg = Str ;
        arity = A1 } ;
      mkPred : Str -> (a1, a2 : Arg) -> LinPred = \nm,a1,a2 -> {
         s = nm ;
         arg1 = a1 ;
         arg2 = a2 ;
         arity = A2 }
      } ;

}
