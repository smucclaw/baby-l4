concrete QuestionsEng of Questions = open Prelude, SyntaxEng in {

  lincat
--    Question = QS ;

    Pred = LinPred ;

  lin

    AreThereAny p = case p.arity of {
      Ar1 => {s = "Are there any" ++ p.s ++ "s?"} ;
      Ar2 => {s = "Are there any" ++ p.arg1 ++ "s who"
               ++ p.s ++ "the" ++ p.arg2 ++ "?"}
      } ;

    AreThereMore p = case p.arity of {
      Ar1 => {s = "Are there more" ++ p.s ++ "s?"} ;
      Ar2 => {s = "Are there more" ++ p.arg1 ++ "s who"
               ++ p.s ++ "the" ++ p.arg2 ++ "?"}
      };

    Properties p = case p.arity of {
      Ar1 => {s = "What's the name of the" ++ p.s ++ "?"} ;
      Ar2 => {s = "What's the name of the" ++ p.arg1 ++ "? \\ What's the name of the" ++ p.arg2 ++ "?"}
      --A2 => {s = "What are the names of" ++ p.arg1 ++ "and"
      --         ++ p.arg2 ++ "that the" ++ p.arg1 ++ p.s ++ "?"}
      } ;

    MkPred1 nm a1 = mkPred nm.s a1.s ;
    MkPred2 nm a1 a2 = mkPred nm.s a1.s a2.s ;

  param
    Arity = Ar1 | Ar2  ;

  oper
    -- To be changed later
    Arg : Type = Str ;
    mkArg : Str -> Arg = \s -> s ;

    -- Dummy constructors that work for any lexicon
    mkName = overload {
      mkName : Str -> SS = ss ;
      mkName : A -> SS = \_ -> ss "adjective" ;
      mkName : N -> SS = \_ -> ss "noun" ;
      mkName : V -> SS = \_ -> ss "verb" ;
      mkName : A2 -> SS = \_ -> ss "adjective2" ;
      mkName : V2 -> SS = \_ -> ss "verb2" ;
      mkName : N2 -> SS = \_ -> ss "noun2"
    } ;
    mkAtom = overload {
      mkAtom : Str -> SS = ss ;
      mkAtom : A -> SS = \_ -> ss "adjective" ;
      mkAtom : N -> SS = \_ -> ss "noun" ;
      mkAtom : V -> SS = \_ -> ss "verb" ;
      mkAtom : A2 -> SS = \_ -> ss "adjective2" ;
      mkAtom : V2 -> SS = \_ -> ss "verb2" ;
      mkAtom : N2 -> SS = \_ -> ss "noun2"
    } ; 

    -- These should work unchanged
    dummyArg : Arg = mkArg "dummy" ;


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
        arity = Ar1 } ;
      mkPred : Str -> (a1, a2 : Arg) -> LinPred = \nm,a1,a2 -> {
         s = nm ;
         arg1 = a1 ;
         arg2 = a2 ;
         arity = Ar2 }
      } ;

}
