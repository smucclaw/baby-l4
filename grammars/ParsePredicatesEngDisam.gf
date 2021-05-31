concrete ParsePredicatesEngDisam of ParsePredicates = ParsePredicatesEng - [AdvNP, AdvVP, AdvCN, CompoundN] ** open Prelude in {

lin
  CompoundN = CompoundNHyphen ;

  AdvNP np adv = np ** {
      s = \\c => "[ (head:" ++ np.s ! c ++ ") (adjunct:" ++ adv.s ++ ") ]"
      } ;

  AdvCN cn adv = cn ** {
      s = \\n,c => "[ (head:" ++ cn.s ! n ! c ++ ") (adjunct:" ++ adv.s ++ ") ]"
      } ;

  -- TODO: Disambiguation brackets need to be done at VPS level?
  -- Standard RGL VP is too terrible to work with
  AdvVP vp adv = vp ** {
    s2 = \\a => vp.s2 ! a ++ "{" ++ adv.s ++ "}";
    isSimple = False ;
  } ;


}