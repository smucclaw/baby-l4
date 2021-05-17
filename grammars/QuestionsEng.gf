concrete QuestionsEng of Questions = AtomsEng - [Pred, LinPred, mkPred] ** open Prelude, SyntaxEng, (S=SyntaxEng), ParadigmsEng, ExtendEng, WordNetEng in {

  lincat
    Question = QS ;
    Pred = LinPred' ;
    -- Pred = AtomsEng.LinPred ** {s : CN} ; -- s field for the name. TODO unify with Answer

  lin

    AreThereAny p = case p.arity of {
      Ar0 => areThere any_Det p.s.cn ;
      Ar1 => areThere any_Det (mkCN p.arg1 (suchThat p.s)) ;
      Ar2 => areThere any_Det (mkCN p.arg1 (suchThat p.s p.arg2))
      } ;

    AreThereMore p = case p.arity of {
      Ar0 => areThere more_Det p.s.cn ;
      Ar1 => areThere more_Det (mkCN p.arg1 (suchThat p.s)) ;
      Ar2 => areThere more_Det (mkCN p.arg1 (suchThat p.s p.arg2))
      } ;


    Properties p = case p.arity of {
      Ar0 => whatName p.s.cn ;
      Ar1 => whatName p.arg1 ;
      Ar2 => whatName p.arg1 p.arg2
      } ;

    -- TODO: unify with Answer.gf ?

    -- decl Rock : Sign
    MkPred0 nm = mkPred' nm ;

    -- intransitive: decl Legal : Business -> Bool
    MkPred1 nm a1 = mkPred' nm a1.cn ;

    -- transitive: decl Win : Player -> Game -> Bool
    -- s = win, arg1 = player, arg2 = game
    MkPred2 nm a1 a2 = mkPred' nm a1.cn a2.cn ;

  oper
    suchThat = overload {
      suchThat : LinAtom -> CN -> RS = \atom,arg ->
        RelVPS which_RP (pred2 atom (mkNP a_Det arg)) ;
      suchThat : LinAtom -> RS = \atom ->
        RelVPS which_RP (pred1 atom)
      } ;

    areThere : Det -> CN -> QS = \det,cn -> mkQS (mkCl (mkNP det cn)) ;

    whatName = overload {
      whatName : CN -> QS = \cn ->
        mkQS (mkQCl whatSg_IP (mkNP the_Det (mkCN name_N2 (mkNP the_Det cn)))) ;
      whatName : (arg1,arg2 : CN) -> QS = \a1,a2 ->
        let args : NP = mkNP and_Conj (mkNP the_Det a1) (mkNP the_Det a2) ;
            names_of_args = mkNP thePl_Det (mkCN name_N2 args) ;
         in mkQS (mkQCl whatPl_IP names_of_args)
      } ;

    name_N2 : N2 = mkN2 (mkN "name") ;

    any_Det = aPl_Det ** {s = "any"} ; -- hack: extending an existing Det from the API
    more_Det = aPl_Det ** {s = "more"} ;

    dummyArg : CN = mkCN (mkN "dummy") ;

    -- Predicates
    LinPred' : Type = {
      s : LinAtom ;
      arg1 : CN ;
      arg2 : CN ;
      arity : Arity ;
      };

    mkPred' = overload {
      mkPred' : LinAtom -> LinPred' = \nm -> {
         s = nm ;
         arg1 = dummyArg ;
         arg2 = dummyArg ;
         arity = Ar0 } ;
      mkPred' : LinAtom -> (a1 : CN) -> LinPred' = \nm,a1 -> {
         s = nm ;
         arg1 = a1 ;
         arg2 = dummyArg ;
         arity = Ar1 } ;
      mkPred' : LinAtom -> (a1, a2 : CN) -> LinPred' = \nm,a1,a2 -> {
         s = nm ;
         arg1 = a1 ;
         arg2 = a2 ;
         arity = Ar2 }
      } ;

}
