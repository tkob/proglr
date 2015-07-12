(* repair.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Representation and pretty-printing of ml-antlr repair actions
 *)

structure AntlrRepair :> sig

    datatype 'tok repair_action
      = Insert of 'tok list
      | Delete of 'tok list
      | Subst of {
          old : 'tok list, 
          new : 'tok list
        }
      | FailureAt of 'tok

    type 'tok repair = AntlrStreamPos.pos * 'tok repair_action

    val actionToString : ('tok -> string) -> 'tok repair_action -> string
    val repairToString : ('tok -> string) -> AntlrStreamPos.sourcemap -> 'tok repair -> string

  end = struct

    datatype 'a repair_action
      = Insert of 'a list
      | Delete of 'a list
      | Subst of {
          old : 'a list, 
          new : 'a list
        }
      | FailureAt of 'a

    type 'a repair = AntlrStreamPos.pos * 'a repair_action

    fun actionToString tokToString repair = let
          val toksToString = (String.concatWith " ") o (map tokToString)
          in
            case repair
             of Insert toks => "try inserting " ^ toksToString toks
              | Delete toks => "try deleting " ^ toksToString toks
              | Subst {old, new} => concat[
                    "try substituting ", toksToString new, " for ", toksToString old
                  ]
              | FailureAt tok => "syntax error at " ^ tokToString tok
            (* end case *)
          end

    fun repairToString tokToString sm (pos, repair) = 
          (AntlrStreamPos.toString sm pos ^ ": " ^ actionToString tokToString repair)

end