
(* utility function *)

(* list as set *)
fun mem x xs = List.exists (fn y => y = x) xs
fun add x xs = if mem x xs then xs else x::xs
fun union [] ys = ys
  | union (x::xs) ys = union xs (add x ys)

fun concatWith _ [] = ""
  | concatWith _ [s] = s
  | concatWith sep (s::ss) = s ^ sep ^ concatWith sep ss

fun dropWhile p [] = []
  | dropWhile p (x::xs) = if p x then dropWhile p xs else x::xs

fun chopDigit s = 
  let
    val cs = rev (String.explode s)
    val cs' = dropWhile Char.isDigit cs
  in
    String.implode (rev cs')
  end

fun toLower s = String.implode (List.map Char.toLower (String.explode s))
fun toUpper s = String.implode (List.map Char.toUpper (String.explode s))

signature INTERN = sig
  type ''a pool
  val emptyPool : ''a pool
  val intern : ''a -> ''a pool -> int * ''a pool
  val internAll : ''a list -> ''a pool -> int list * ''a pool
  val present : int -> ''a pool -> bool
  val valueOf : int -> ''a pool -> ''a
  val numbersOf : ''a pool -> int list
  val toList : ''a pool -> (int * ''a) list
end

structure Intern :> INTERN = struct
  type ''a pool = (int * ''a) list

  val emptyPool  = []
  fun nextNumber [] = 0
    | nextNumber ((number, _)::values) = number + 1
  fun intern value pool =
    case List.find (fn (_, value') => value = value') pool of
      SOME (number, _) => (number, pool)
    | NONE =>
      let
        val number = nextNumber pool
        val pool' = (number, value)::pool
      in
        (number, pool')
      end
  fun internAll values pool =
    let
      fun loop [] pool numbers = (rev numbers, pool)
        | loop (value::values) pool numbers =
            let val (number, pool') = intern value pool in
              loop values pool' (number::numbers)
            end
    in
      loop values pool []
    end
  fun present number [] = false
    | present number ((number', _)::pool) =
        number = number' orelse present number pool
  fun valueOf number pool =
    let
      val (_, value) = valOf (List.find (fn (number', _) => number = number') pool)
    in
      value
    end
  fun numbersOf pool = List.map (fn (n, _) => n) pool
  fun toList pool = pool
end

(* grammatical symbols: terminal and non-terminal *)
signature SYMBOL = sig
  eqtype symbol
  datatype attr_type = Unit | Int | Str | Char
  datatype kind = TERM of attr_type | NONTERM
  val isTerm : symbol -> bool
  val attrOf : symbol -> attr_type
  val show : symbol -> string
  val makeSymbols : (string * attr_type) list * string list -> (symbol list * symbol list)
  val S' : symbol
  val EOF : symbol
end

structure Symbol :> SYMBOL where type symbol = int = struct
  type symbol = int
  type name = string
  datatype attr_type = Unit | Int | Str | Char
  datatype kind = TERM of attr_type | NONTERM

  val S' = 0
  val EOF = 1
  val symbols : (string * kind) vector ref = ref (Vector.fromList [])
  fun makeSymbols (terms, nonterms) =
    let
      val numTerms = length terms
      val numNonterms = length nonterms
      val terms' = map (fn (name, attrType) => (name, TERM attrType)) terms
      val nonterms' = map (fn name => (name, NONTERM)) nonterms
      val symbols' = [("S'", NONTERM), ("EOF", TERM Unit)] @ terms' @ nonterms'
    in
      (symbols := (Vector.fromList symbols');
      (List.tabulate (numTerms, (fn n => 2 + n)),
       List.tabulate (numNonterms, (fn n => 2 + numTerms + n ))))
    end

  fun lookup symbol =
    SOME (Vector.sub (!symbols, symbol))
    handle Subscript => NONE

  fun show symbol =
    case lookup symbol of
      SOME (name, _) => name
    | NONE => raise Fail "symbol not found"

  fun isTerm symbol =
    case lookup symbol of
      SOME (_, TERM _) => true
    | SOME (_, NONTERM) => false
    | NONE => raise Fail "symbol not found"

  fun attrOf symbol =
    case lookup symbol of
      SOME (_, TERM attrType) => attrType
    | SOME (_, NONTERM) => raise Fail ("symbol " ^ show symbol ^ " is nonterm")
    | NONE => raise Fail "symbol not found"
end

signature GRAMMAR = sig
  eqtype constructor
  type rule
  type grammar

  val makeRule : constructor * Symbol.symbol * Symbol.symbol list -> rule
  val makeGrammar : Symbol.symbol list -> Symbol.symbol list -> (string option * Symbol.symbol * Symbol.symbol list) list -> Symbol.symbol -> grammar
  val rulesOf : grammar -> rule list
  val consOf : rule -> constructor
  val lhsOf : rule -> Symbol.symbol
  val rhsOf : rule -> Symbol.symbol list
  val startSymbolOf : grammar -> Symbol.symbol
  val termsOf : grammar -> Symbol.symbol list
  val nontermsOf : grammar -> Symbol.symbol list
  val attrOf : Symbol.symbol -> (Symbol.symbol * Symbol.attr_type) list -> Symbol.attr_type

  val isConsDefined : constructor -> bool
  val showCons : constructor -> string

  val showRule : rule -> string
  val printGrammar : grammar -> unit
end

structure Grammar :> GRAMMAR where
  type constructor = string option
  = struct
  type constructor = string option
  local
    open Symbol
    type lhs = symbol
    type rhs = symbol list
    type terms = symbol list
    type nonterms = symbol list
    type start = symbol
  in
    type rule = constructor * lhs * rhs
    type grammar = terms * nonterms * rule list * start
  end

  fun makeRule (rule as (constructor, lhs, rhs)) =
    if Symbol.isTerm lhs then raise Fail "non-terminal cannot be lhs of a rule"
    else rule
  fun makeGrammar terms nonterms rules startSymbol =
    let
      val rules = List.map makeRule rules
    in
      (terms, nonterms, rules, startSymbol)
    end
  fun rulesOf (_, _, rules, _) = rules
  fun consOf (constructor, _, _) = constructor
  fun lhsOf (_, lhs, _) = lhs
  fun rhsOf (_, _, rhs) = rhs
  fun startSymbolOf (_, _, _, startSymbol) = startSymbol
  fun termsOf (terms, _, _, _) = terms
  fun nontermsOf (_, nonterms, _, _) = nonterms
  fun attrOf symbol terms =
    #2 (valOf (List.find (fn (symbol', attr) => symbol = symbol') terms))

  val isConsDefined = Option.isSome
  fun showCons (SOME s) = s | showCons NONE = "_"
  fun showRule (con, lhs, rhs) =
    showCons con ^ ". "
      ^ Symbol.show lhs ^ " ::= "
      ^ concatWith " " (List.map Symbol.show rhs) ^ ";"
  fun printGrammar (_, _, rules, _) =
    let
      fun printRule rule = print (showRule rule ^ "\n")
    in
      List.app printRule rules
    end
end

signature LRITEM = sig
  eqtype item
  type items = item list
  datatype kind = Shift | Reduce | ShiftReduce | ReduceReduce
  val fromRule : Grammar.rule -> item
  val expand : items -> Grammar.rule list -> items
  val moveOver : items -> Symbol.symbol -> Grammar.rule list -> items
  val nextSymbols : items -> Symbol.symbol list
  val kindOf : items -> kind
  val consOf : item -> Grammar.constructor
  val lhsOf : item -> Symbol.symbol
  val rhsBeforeDot : item -> Symbol.symbol list
  val show : item -> string
end

structure LrItem :> LRITEM = struct
  local
    open Symbol
    type lhs = symbol
    type rhs_before_dot = symbol list
    type rhs_after_dot = symbol list
  in
    type item = Grammar.constructor * lhs * rhs_before_dot * rhs_after_dot
    type items = item list
  end

  datatype kind = Shift | Reduce | ShiftReduce | ReduceReduce

  fun fromRule rule = (Grammar.consOf rule, Grammar.lhsOf rule, [], Grammar.rhsOf rule)

  fun expand (lrItems : items) (rules : Grammar.rule list) =
    let
      (* 1st = unexpanded items, 2nd = expanded items *)
      fun loop [] expanded = expanded
        | loop (lrItem::lrItems) expanded =
            if mem lrItem expanded then loop lrItems expanded
            else
              case lrItem of
                (* if the dot is not in fromt of a non-terminal *)
                (_, _, _, [])     => loop lrItems (lrItem::expanded)
              | (_, _, _, sym::_) =>
                  if Symbol.isTerm sym then
                    (* if the dot is not in fromt of a non-terminal *)
                    loop lrItems (lrItem::expanded)
                  else
                    (* if the dot is in fromt of a non-terminal *)
                    let
                     (* all grammar rules of the form sym->... *)
                      val rules = List.filter (fn rule => Grammar.lhsOf rule = sym) rules
                     (* convert the rules to LR items *)
                      val newLrItems = map fromRule rules
                    in
    		  (* lrItem is expanded now, since it generated new items.
    		     The new items are possibly unexpanded. *)
                      loop (newLrItems @ lrItems) (lrItem::expanded)
                    end
    in
      loop lrItems []
    end

  fun moveOver items symbol grammar =
    let
      fun move (c, n, l, next::rest) =
        if next = symbol then SOME (c, n, l @ [next], rest)
        else NONE
        | move (c, n, l, []) = (
          print (case c of SOME c => c ^ "\n" | NONE => "");
          print (Symbol.show n ^ "\n");
          List.app (fn s => print (Symbol.show s ^ " ")) l;
          raise Match
          )


      val moved = List.mapPartial move items
    in
      expand moved grammar
    end

  fun nextSymbols lrItems =
    let
      fun loop [] symbols = symbols
        | loop ((_, _, _, [])::lrItems) symbols = loop lrItems symbols
        | loop ((_, _, _, nextSymbol::_)::lrItems) symbols =
            loop lrItems (add nextSymbol symbols)
    in
      loop lrItems []
    end

  fun kindOf items =
    let
      fun endsWithDot (_, _, _, []) = true
        | endsWithDot _ = false
    in
      if List.all endsWithDot items then
        if length items = 1 then Reduce
        else ReduceReduce
      else
        if List.exists endsWithDot items then ShiftReduce
        else Shift
    end

  fun consOf (cons, _, _, _) = cons
  fun lhsOf (_, lhs, _, _) = lhs
  fun rhsBeforeDot (_, _, rhs, []) = rhs

  fun show (_, lhs, rhs1, rhs2) =
    Symbol.show lhs ^ " -> "
      ^ concatWith " " (List.map Symbol.show rhs1)
      ^ " . "
      ^ concatWith " " (List.map Symbol.show rhs2)
end

structure State = struct
  datatype state = Accepting of LrItem.item * Grammar.constructor | NonAccepting of LrItem.items
end

signature AUTOMATON = sig
  type state
  type state_number = int
  eqtype alphabet
  type transition = state_number * alphabet * state_number
  type automaton
  val makeAutomaton : Grammar.grammar -> automaton
  val stateNumbers : automaton -> state_number list
  val stateOf : state_number -> automaton -> state
  val nextStatesOf : state_number -> automaton -> (alphabet * state_number) list
  val numbersAndStates : automaton -> (state_number * state) list
  val isAccepting : state_number -> automaton -> bool
  val printAutomaton : automaton -> unit
end

structure Automaton :> AUTOMATON where
  type state = State.state
  and type alphabet = Symbol.symbol
  = struct
  open State
  type state = State.state
  type state_number = int
  type alphabet = Symbol.symbol
  type transition = state_number * alphabet * state_number
  type automaton = LrItem.items Intern.pool * transition list

  fun stateOfLrItems lrItems =
    case LrItem.kindOf lrItems of
      LrItem.Shift => NonAccepting lrItems
    | LrItem.Reduce =>
        let val lrItem = hd lrItems in
          Accepting (lrItem, LrItem.consOf lrItem)
        end
    | _ => raise Fail "conflict"

  fun makeAutomaton grammar =
    let
      val startRule = Grammar.makeRule (NONE , Symbol.S', [Grammar.startSymbolOf grammar, Symbol.EOF])
      val rules = startRule::Grammar.rulesOf grammar
      val startState = LrItem.expand [LrItem.fromRule startRule] rules
      val (startStateNumber, pool) = Intern.intern startState Intern.emptyPool
  
      (* loop U S T
           where U = numbers of unprocessed state, 
                 S = a list of states and numbers,
                 T = trnasitions *)
      fun loop [] pool transitions = (pool, transitions)
        | loop (number::numbers) pool transitions =
            let
              val state = Intern.valueOf number pool
              val nextSymbols = LrItem.nextSymbols state
              val nextStates = map (fn symbol => LrItem.moveOver state symbol rules) nextSymbols
              val (nextStateNumbers, pool') = Intern.internAll nextStates pool
              (* State numbers which are not present in old S are new *)
              val newStateNumbers = List.filter (fn number => not (Intern.present number pool)) nextStateNumbers
              val newTransitions =
                map
                  (fn (symbol, nextStateNumber) => (number, symbol, nextStateNumber))
                  (ListPair.zip (nextSymbols, nextStateNumbers))
            in
              loop (newStateNumbers @ numbers) pool' (newTransitions @ transitions)
            end
    in
      loop [startStateNumber] pool []
    end
  fun stateNumbers (pool, _) = Intern.numbersOf pool
  fun stateOf number (pool, _) = stateOfLrItems (Intern.valueOf number pool)
  fun numbersAndStates (pool, _) = List.map (fn (n, items) => (n, stateOfLrItems items)) (Intern.toList pool)
  fun nextStatesOf state (_, transitions) =
    List.map (fn (_, symbol, next) => (symbol, next)) (List.filter (fn (s', _, _) => state = s') transitions)

  fun isAccepting stateNumber automaton =
    case stateOf stateNumber automaton of
      Accepting _ => true
    | NonAccepting _ => false

  fun printStates states =
    let
      fun showState state = concatWith " | " (List.map LrItem.show state)
      fun printState (n, state) = print (Int.toString n ^ ": " ^ showState state ^ "\n")
    in
      List.app printState (Intern.toList states)
    end
  
  fun printTransitions transitions =
    let
      fun printTransition (s1, symbol, s2) =
        print (Int.toString s1 ^ " -> " ^ Symbol.show symbol ^ " -> " ^ Int.toString s2 ^ "\n")
    in
      List.app printTransition transitions
    end

  fun printAutomaton (states, transitions) =
    (printStates states;
    printTransitions transitions)
end

structure MLAst = struct
  type ident = string
  type tycon = string
  
  datatype
      ty =
        Tycon of tycon
      | TupleType of ty list
      | AsisType of string
  and strexp = Struct of strdec list
  and strdec = 
        Structure of strbind
      | Dec of dec
  and sigdec = Signature of sigbind
  and dec =
        Datatype of datbind list
      | Fun of fvalbind list
      | Val of pat * exp
      | AsisDec of string
  and exp =
        AsisExp of string
      | Let of dec list * exp
      | Case of exp * mrule list
      | TupleExp of exp list
      | AppExp of exp * exp
  and pat = AsisPat of string
  and fundec = Functor of funbind
  and sigexp =
        Sig of spec list
      | SigId of ident * (ident * ty) list
  and spec =
        ValSpec of valdesc
      | TypeSpec of typedesc
  withtype
      datbind = ident * (ident * ty option) list
  and fvalbind = ident * (pat list * exp) list
  and strbind = (ident * strexp) list
  and sigbind = (ident * sigexp) list
  and mrule = pat * exp
  and funbind = (ident * ident * sigexp * strexp) list
  and valdesc = (ident * ty) list
  and typedesc = ident list

  fun p out s = (TextIO.output (out, s); TextIO.flushOut out)
  fun printIndent out 0 = ()
    | printIndent out n = (p out " "; printIndent out (n - 1))
  fun out outs indent str = (printIndent outs indent; p outs str; p outs "\n")

  exception BlockExp
  fun showTy (Tycon tycon) = tycon
    | showTy (TupleType tys) =
      let
        fun prepend [] = "unit"
          | prepend [ty] = showTy ty
          | prepend (ty::tys) = (showTy ty) ^ " * " ^ prepend tys
      in
        prepend tys
      end
    | showTy (AsisType ty) = ty
  fun showPat (AsisPat string) = string
  fun showExp (AsisExp string) = string
    | showExp (Let _) = raise BlockExp
    | showExp (Case _) = raise BlockExp
    | showExp (TupleExp []) = "()"
    | showExp (TupleExp (first::rest)) =
      let
        fun addComma exp = ", " ^ showExp exp
      in
        "(" ^ showExp first ^ concat (map addComma rest) ^ ")"
      end
    | showExp (AppExp (e1, e2)) = "(" ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
  fun showSigExp (SigId (sigid, [])) = sigid
    | showSigExp (SigId (sigid, first::rest)) =
      let
        fun showWh (t, tycon) = " type " ^ t ^ " = " ^ (showTy tycon)
      in
        sigid ^ " where" ^ (showWh first)
        ^ List.foldl (fn (wh, acc) => acc ^ " and" ^ showWh wh) "" rest
      end
  fun printExp outs indent (exp as AsisExp string) = out outs indent (showExp exp)
    | printExp outs indent (Let (decs, exp)) =
      (out outs indent "let";
      List.app (printDec outs (indent + 2)) decs;
      out outs indent "in";
      printExp outs (indent + 2) exp;
      out outs indent "end")
    | printExp outs indent (Case (exp, mrules)) =
      let
        fun printMrule indent pre (pat, exp) =
          out outs indent (pre ^ showPat pat ^ " => " ^ showExp exp)
        fun printMrules indent [] = ()
          | printMrules indent (first::rest) =
          (printMrule indent "  " first;
          List.app (printMrule indent "| ") rest)
      in
        out outs indent ("case " ^ showExp exp ^ " of");
        printMrules indent mrules
      end
    | printExp outs indent exp = out outs indent (showExp exp)
  and printDec outs indent (Datatype []) = ()
    | printDec outs indent (Datatype (first::rest)) =
      let
        fun printConbind indent pre (vid, ty) =
          out outs indent (pre ^ vid ^ (case ty of NONE => "" | SOME ty => " of " ^ showTy ty))
        fun printConbinds indent [] = ()
          | printConbinds indent (first::rest) =
          (printConbind indent "  " first;
  	  List.app (printConbind indent "| ") rest)
        fun printDatbind indent pre (tycon, conbind) =
          (out outs indent (pre ^ " " ^ tycon ^ " =");
          printConbinds indent conbind)
      in
        (printDatbind indent "datatype" first;
        List.app (printDatbind indent "and") rest)
      end
    | printDec outs indent (Fun []) = ()
    | printDec outs indent (Fun (first::rest)) =
      let
        fun printFvalbind indent pre (ident, []) = out outs indent (pre ^ " " ^ ident ^ " _ = raise Fail \"unimplemented\"")
          | printFvalbind indent pre (ident, (first::rest)) =
          let
            fun printClause indent pre (patseq, exp) =
              let
                val patterns = List.foldl (fn (a,b) => b ^ " " ^ showPat a) "" patseq
              in
                let val expstr = showExp exp in
                  if (indent + String.size ident + String.size patterns + String.size expstr) >  70 then
                    raise BlockExp
                  else
                    out outs indent (pre ^ " " ^ ident ^ patterns ^ " = " ^ expstr)
                end 
                handle BlockExp =>
                  (out outs indent (pre ^ " " ^ ident ^ patterns ^ " =");
                  printExp outs (indent + 4) exp)
              end
          in
            printClause indent pre first;
            List.app (printClause indent "  |") rest
          end
      in
        (printFvalbind indent "fun" first;
        List.app (printFvalbind indent "and") rest)
      end
    | printDec outs indent (AsisDec s) = out outs indent s
  fun printStrdec outs indent (Structure []) = ()
    | printStrdec outs indent (Structure (first::rest)) =
      let
        fun printStrbind indent pre (ident, Struct strdecseq) =
          (out outs indent (pre ^ " " ^ ident ^ " = struct");
          List.app (printStrdec outs (indent + 2)) strdecseq;
          out outs indent "end")
      in
        printStrbind indent "structure" first;
        List.app (printStrbind indent "and") rest
      end
    | printStrdec outs indent (Dec dec) = printDec outs indent dec
  fun printSpec outs indent (ValSpec []) = ()
    | printSpec outs indent (ValSpec (first::rest)) =
      let
        fun printValSpec pre (ident, ty) =
          out outs indent (pre ^ " " ^ ident ^ " : " ^ showTy ty)
      in
        (printValSpec "val" first;
        List.app (printValSpec "and") rest)
      end
    | printSpec outs indent (TypeSpec []) = ()
    | printSpec outs indent (TypeSpec (first::rest)) =
      let
        fun printTypeSpec pre ty =
          out outs indent (pre ^ " " ^ ty)
      in
        (printTypeSpec "type" first;
        List.app (printTypeSpec "and") rest)
      end
  fun printSigdec outs indent (Signature []) = ()
    | printSigdec outs indent (Signature (first::rest)) =
      let
        fun printSigbind indent pre (ident, Sig specs) =
          (out outs indent (pre ^ " " ^ ident ^ " = sig");
          List.app (printSpec outs (indent + 2)) specs;
          out outs indent "end")
      in
        printSigbind indent "signature" first;
        List.app (printSigbind indent "and") rest
      end
  fun printFundec outs indent (Functor []) = ()
    | printFundec outs indent (Functor (first::rest)) =
      let
        fun printFunbind indent pre (funid, sigid, sigexp, Struct strdecs) =
          (out outs indent (pre ^ " " ^ funid ^ "(" ^ sigid ^ " : " ^ showSigExp sigexp ^ ") = struct");
          List.app (printStrdec outs (indent + 2)) strdecs;
          out outs indent "end")
          
      in
        printFunbind indent "functor" first;
        List.app (printFunbind indent "and") rest
      end
end

structure CodeGenerator = struct

  fun fromAttr Symbol.Unit = NONE
    | fromAttr Symbol.Int = SOME (MLAst.Tycon "int")
    | fromAttr Symbol.Str = SOME (MLAst.Tycon "string")
    | fromAttr Symbol.Char = SOME (MLAst.Tycon "char")

  fun makeTokenDatatype typeName tokens =
    let
      fun f symbol = (Symbol.show symbol, fromAttr (Symbol.attrOf symbol))
      fun makeTycons tokens = List.map f tokens
    in
      MLAst.Datatype [(typeName, makeTycons tokens)]
    end
  fun makeShowFun tokens =
    let
      fun makePat symbol = 
        if Symbol.isTerm symbol then
          if Symbol.attrOf symbol = Symbol.Unit then 
            MLAst.AsisPat ("(" ^ Symbol.show symbol ^ ")")
          else
            MLAst.AsisPat ("(" ^ Symbol.show symbol ^ " a)")
        else
          MLAst.AsisPat ("(" ^ Symbol.show symbol ^ " _)")
      fun makeBody symbol =
        if Symbol.isTerm symbol then
          case Symbol.attrOf symbol of
            Symbol.Unit => MLAst.AsisExp ("\"" ^ Symbol.show symbol ^ "\"")
          | Symbol.Int  => MLAst.AsisExp ("\"" ^ Symbol.show symbol ^ "(\" ^ Int.toString a ^ \")\"")
          | Symbol.Str  => MLAst.AsisExp ("\"" ^ Symbol.show symbol ^ "(\" ^ a ^ \")\"")
          | Symbol.Char => MLAst.AsisExp ("\"" ^ Symbol.show symbol ^ "(\" ^ Char.toString a ^ \")\"")
        else
          MLAst.AsisExp ("\"" ^ Symbol.show symbol ^ "\"")
      val patExps = List.map (fn token => ([makePat token], makeBody token)) tokens
    in
      MLAst.Fun [("show", patExps)]
    end
  fun nt2dt nonterm = toLower (chopDigit (Symbol.show nonterm))
  fun makeAstDatatype datatypeNames rules terms =
    let
      fun makeDatatype name =
        let
          fun ruleFor name = List.filter (fn rule => name = nt2dt (Grammar.lhsOf rule) andalso Option.isSome (Grammar.consOf rule)) rules
          fun symToTycon sym = 
            if Symbol.isTerm sym then
              fromAttr (Symbol.attrOf sym)
            else
              SOME (MLAst.Tycon (nt2dt sym))
          fun f rhs =
            case List.mapPartial symToTycon rhs of
              [ty] => ty
            | tys => MLAst.TupleType tys
        in
          (* type name and constructors *)
          (name,
          List.map (fn rule => (valOf (Grammar.consOf rule), SOME (f (Grammar.rhsOf rule)))) (ruleFor name))
        end
    in
      (* this makes mutually recursive datatypes *)
      MLAst.Datatype (List.map makeDatatype datatypeNames)
    end
  fun makeCategoryDatatype typeName symbols =
    let
      fun f symbol =
        let val name = Symbol.show symbol in
          if Symbol.isTerm symbol then
            (name, fromAttr (Symbol.attrOf symbol))
          else
            (name, SOME (MLAst.Tycon ("Ast." ^ nt2dt symbol)))
        end
      fun makeTycons tokens = List.map f tokens
    in
      MLAst.Datatype [(typeName, makeTycons symbols)]
    end
  fun makeFromTokenFun tokens =
    let
      fun makePat symbol =
        if Symbol.attrOf symbol = Symbol.Unit then
          MLAst.AsisPat ("(Token." ^ Symbol.show symbol ^ ")")
        else
          MLAst.AsisPat ("(Token." ^ Symbol.show symbol ^ " a)")
      fun makeBody symbol =
        if Symbol.attrOf symbol = Symbol.Unit then
          MLAst.AsisExp (Symbol.show symbol)
        else
          MLAst.AsisExp (Symbol.show symbol ^ " a")
      val patExps = List.map (fn token => ([makePat token], makeBody token)) tokens
    in
      MLAst.Fun [("fromToken", patExps)]
    end

  fun holdSv sym =
    not (Symbol.isTerm sym) orelse Symbol.attrOf sym <> Symbol.Unit
  (* st functions *)
  fun makeStMrule automaton (symbol, next) =
    let
      val pat = if holdSv symbol
                then MLAst.AsisPat (Symbol.show symbol ^ " _")
                else MLAst.AsisPat (Symbol.show symbol)
      val exp = MLAst.AsisExp
        ("st" ^ Int.toString next ^ 
        (if Automaton.isAccepting next automaton
        then " (stackItem::stack) strm"
        else " (stackItem::stack) next strm'"))
    in
      (pat, exp)
    end
  fun makeStFvalbind automaton stateNumber =
    let
      val n = Int.toString stateNumber
    in
      case Automaton.stateOf stateNumber automaton of
        State.Accepting (item, _) =>
          let
            val cons = LrItem.consOf item
            val lhs = LrItem.lhsOf item
            val rhs = LrItem.rhsBeforeDot item
            val index = ref 0
            val stackPat =
              List.foldl
              (fn (sym, pats) =>
                let
                  val n = Int.toString (!index)
                  val sv = if holdSv sym then SOME ("sv" ^ n) else NONE
                in
                  index := !index + 1;
                  (sym, sv, "stNum" ^ n, "pos"^ n)::pats
                end)
              []
              rhs
            val stackPatString =
              let
                fun toString (sym, sv, stNum, pos) =
                  "("
                  ^ Symbol.show sym
                  ^ (case sv of SOME sv => " " ^ sv | NONE => "")
                  ^ ", "
                  ^ pos
                  ^ ", "
                  ^ stNum
                  ^ ")::"
              in
                concat (map toString stackPat)
              end
            val svalues = rev (List.mapPartial #2 stackPat)
            val svaluesAst =
              case cons of 
                SOME c => MLAst.AppExp (MLAst.AsisExp ("Ast." ^ c), MLAst.TupleExp (map MLAst.AsisExp svalues))
              | NONE => MLAst.TupleExp (map MLAst.AsisExp svalues)
            val currentAst = MLAst.TupleExp [MLAst.AppExp (MLAst.AsisExp (Symbol.show lhs), svaluesAst), MLAst.AsisExp "pos0"]
          in
            ("st" ^ n, [
              ([MLAst.AsisPat ("(" ^ stackPatString ^ "stack)"),
                MLAst.AsisPat "strm"],
                if lhs = Symbol.S' then
                  MLAst.AsisExp "sv0"
                else
                  MLAst.AsisExp ("go stNum0 stack "
                    ^ MLAst.showExp currentAst
                    ^ " strm"))
            ])
          end
      | State.NonAccepting items =>
          let
            val nextStates = Automaton.nextStatesOf stateNumber automaton
            val lastMrule = (MLAst.AsisPat "c", MLAst.AsisExp ("raise Parse (c, pos, " ^ n ^ ")"))
            val stMrules = List.map (makeStMrule automaton) nextStates @ [lastMrule]
          in
            ("st" ^ n, [
              (map MLAst.AsisPat ["stack", "(category, pos)", "strm"],
              MLAst.Let ([
                MLAst.AsisDec "val (next, strm') = shift strm",
                MLAst.AsisDec ("val stackItem = (category, pos, " ^ n ^ ")")],
                MLAst.Case (MLAst.AsisExp "category", stMrules)))
            ])
          end
    end

  fun generateParser outs grammar =
    let
      val tokens = Symbol.EOF :: Grammar.termsOf grammar
      val nonterms = Grammar.nontermsOf grammar
      val rules = Grammar.rulesOf grammar
      val categories = tokens @ nonterms
      val automaton = Automaton.makeAutomaton grammar
      val numbersAndStates = Automaton.numbersAndStates automaton
      val stateNumbers = List.map #1 numbersAndStates
      val accepting = List.filter (fn (n,s) => Automaton.isAccepting n automaton) numbersAndStates;
    
      (* Token *)
      val tokenDatatype = makeTokenDatatype "token" tokens
      val tokenShowFun = makeShowFun tokens
      val tokenStructure =
        MLAst.Structure [("Token", MLAst.Struct
          [MLAst.Dec tokenDatatype,
           MLAst.Dec tokenShowFun])]
    
      (* Aat *)
      val astDatatypeNames = List.foldr (fn (nonterm, datatypeNames) => add (nt2dt nonterm) datatypeNames) [] nonterms
      val astDatatype = makeAstDatatype astDatatypeNames rules tokens
      val astStructure =
        MLAst.Structure [("Ast", MLAst.Struct [MLAst.Dec astDatatype])]
    
      (* Category *)
      val categoryDatatype = makeCategoryDatatype "category" categories
      val categoryShowFun = makeShowFun categories
      val fromTokenFun = makeFromTokenFun tokens
      val categoryStructure =
        MLAst.Structure [("Category", MLAst.Struct
          [MLAst.Dec categoryDatatype,
           MLAst.Dec categoryShowFun,
           MLAst.Dec fromTokenFun])]
    
      (* go function *)
      val nonacceptingStateNumbers = List.filter (fn number => not (Automaton.isAccepting number automaton)) stateNumbers
      val stateNumbersAsString = List.map Int.toString nonacceptingStateNumbers
      val goMrules = List.map (fn n => (MLAst.AsisPat n, MLAst.AsisExp ("st" ^ n ^ " stack current strm"))) stateNumbersAsString
      val goCase = MLAst.Case (MLAst.AsisExp "stateNumber", goMrules)
      val goFvalbind =
        ("go", [
          (map MLAst.AsisPat ["stateNumber", "stack", "current", "strm"], goCase)
        ])
    
      val shiftFun = MLAst.Fun [
        ("shift", [([MLAst.AsisPat "strm"],
          MLAst.Let (
            [MLAst.AsisDec "val pos = Lex.getPos strm",
             MLAst.AsisDec "val (token, span, strm') = Lex.lex sourcemap strm"],
            MLAst.AsisExp "((Category.fromToken token, pos), strm')"))])]
      val st = List.map (makeStFvalbind automaton) (Automaton.stateNumbers automaton)
      (* state machine function *)
      val stFuns = MLAst.Fun (goFvalbind::st)
    
      val lexSignature = MLAst.Signature [("Lex",
        MLAst.Sig [
          MLAst.TypeSpec ["strm"],
          MLAst.TypeSpec ["pos"],
          MLAst.TypeSpec ["tok"],
          MLAst.ValSpec [("lex", MLAst.AsisType "AntlrStreamPos.sourcemap -> strm -> tok * AntlrStreamPos.span * strm")],
          MLAst.ValSpec [("getPos", MLAst.AsisType "strm -> pos")]])]

      val parseFun = MLAst.Fun [
        ("parse", [([MLAst.AsisPat "sourcemap", MLAst.AsisPat "strm"],
          MLAst.Let (
            [shiftFun, stFuns, MLAst.AsisDec "val (next, strm') = shift strm"],
            MLAst.AsisExp "st0 [] next strm'"))])]
    
      val parseStructure = MLAst.Struct [
        MLAst.Dec (MLAst.AsisDec "open Category"),
        MLAst.Dec (MLAst.AsisDec "exception Parse of category * Lex.pos * int"),
        MLAst.Dec parseFun
      ]
    
      val parseFunctor = MLAst.Functor [(
        "Parse",
        "Lex",
        MLAst.SigId ("Lex", [("tok", MLAst.Tycon "Token.token")]),
        parseStructure)]
    in
      MLAst.printStrdec outs 0 tokenStructure;
      MLAst.printStrdec outs 0 astStructure;
      MLAst.printStrdec outs 0 categoryStructure;
      MLAst.printSigdec outs 0 lexSignature;
      MLAst.printFundec outs 0 parseFunctor
    end
end

local
  open Symbol
  val ([INT, LPAREN, RPAREN, SUB, DOLLAR], [S, E0, E1]) =
    Symbol.makeSymbols
      ([("INT", Int),
        ("LPAREN", Unit),
        ("RPAREN", Unit),
        ("SUB", Unit),
        ("DOLLAR", Unit)
       ],
       ["S", "E0", "E1"])
in
  val grammar = Grammar.makeGrammar
    [INT, LPAREN, RPAREN, SUB, DOLLAR] 
    [S, E0, E1]
    [
      (SOME "ExpStmt", S,  [E0, DOLLAR]),
      (SOME "SubExp" , E0, [E0, SUB, E1]),
      (NONE          , E0, [E1]),
      (SOME "EInt"   , E1, [INT]),
      (NONE          , E1, [LPAREN, E0, RPAREN])]
    S
end

fun main () =
  let
    val args = CommandLine.arguments ()
    val (parserFileName::_) = args
    val parserOutStream = TextIO.openOut parserFileName
  in
    CodeGenerator.generateParser parserOutStream grammar
    before
      TextIO.closeOut parserOutStream
  end
  handle Bind =>
    raise Fail ("usage: mlbnfc outputFilename")

