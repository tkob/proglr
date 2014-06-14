
structure Parse = ParseFun(Lexer)

structure Util = struct
  (* list as set *)
  fun mem x xs = List.exists (fn y => y = x) xs
  fun add (x, xs) = if mem x xs then xs else x::xs
  fun union [] ys = ys
    | union (x::xs) ys = union xs (add (x, ys))
  
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
end

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

signature GRAMMAR = sig
  datatype constructor = Id of string | Wild  | ListE | ListCons | ListOne
  type rule
  type grammar

  datatype kind = Nonterm | UnitTerm | IntTerm | StrTerm | CharTerm | RealTerm
  eqtype symbol

  val fromAst : Parse.Ast.grammar -> grammar
  val makeRule : constructor * symbol * symbol list -> rule

  val rulesOf : grammar -> rule list

  val consOf : rule -> constructor
  val lhsOf : rule -> symbol
  val rhsOf : rule -> symbol list

  val startSymbolOf : grammar -> symbol

  val termsOf : grammar -> symbol list
  val nontermsOf : grammar -> symbol list

  val isConsDefined : constructor -> bool
  val showCons : constructor -> string

  val showRule : rule -> string
  val printGrammar : grammar -> unit

  val showSymbol : symbol -> string
  val isTerm : symbol -> bool
  val kindOf : symbol -> kind
  val levelOf : symbol -> int
  val identOfSymbol : symbol -> string
  val S' : symbol
  val EOF : symbol
end

structure Grammar :> GRAMMAR = struct
  structure Handle :> HASHABLE where type t = string * int = struct
    (* handle for grammatical symbol.
       T = ("T", 0), [T] = ("T", 1), [[T]] = ("T", 2), ... *)
    type t = string * int
    fun eq (a, b) = a = b
    fun hash (ident, level) =
      let
        val word8ToWord = Word.fromLarge o Word8.toLarge
        fun hash (ch, h) = JenkinsHash.hashInc h (word8ToWord ch)
      in
        Word8Vector.foldl hash (Word.fromInt level) (Byte.stringToBytes ident)
      end
  end
  structure SymbolHashTable = HashTable(structure Key = Handle)

  datatype kind = Nonterm | UnitTerm | IntTerm | StrTerm | CharTerm | RealTerm
  type symbol = Handle.t * kind

  datatype constructor = Id of string | Wild  | ListE | ListCons | ListOne
  type lhs = symbol
  type rhs = symbol list
  type rule = constructor * lhs * rhs
  type grammar = {
         terms : symbol list,
         nonterms : symbol list,
         rules : rule list,
         start : symbol}

  fun isTerm (_, Nonterm) = false
    | isTerm (_, _) = true
  fun showSymbol ((ident, 0), _) = ident
    | showSymbol ((ident, level), kind) = "[" ^ showSymbol ((ident, level - 1), kind) ^ "]"
  fun kindOf (_, kind) = kind
  fun levelOf ((_, level), _) = level
  fun identOfSymbol ((ident, _), _) = ident
  val S' = (("S'", 0), Nonterm)
  val EOF = (("EOF", 0), UnitTerm)

  fun makeRule (rule as (constructor, lhs, rhs)) =
    if isTerm lhs then raise Fail "non-terminal cannot be lhs of a rule"
    else rule

  fun rulesOf ({rules,...} : grammar) = rules

  fun consOf (constructor, _, _) = constructor
  fun lhsOf (_, lhs, _) = lhs
  fun rhsOf (_, _, rhs) = rhs

  fun fromAst ast =
    let
      val table = SymbolHashTable.table 256
      fun catToHandle (Parse.Ast.IdCat (_, ident)) = (ident, 0)
        | catToHandle (Parse.Ast.ListCat (_, cat)) =
            let val (ident, level) = catToHandle cat in
              (ident, level + 1)
            end
      fun itemToHandle (Parse.Ast.Terminal (_, str)) = (str, ~1)
        | itemToHandle (Parse.Ast.NTerminal (_, cat)) = catToHandle cat
      fun labelToCons (Parse.Ast.Id (_, ident)) = Id ident
        | labelToCons (Parse.Ast.Wild _) = Wild
        | labelToCons (Parse.Ast.ListE _) = ListE
        | labelToCons (Parse.Ast.ListCons _) = ListCons
        | labelToCons (Parse.Ast.ListOne _) = ListOne
      val terms =
        let
          fun termsOfGrammar (Parse.Ast.Grammar (_, tokens, _)) terms =
                termsOfTokens tokens terms
          and termsOfTokens (Parse.Ast.NilToken _) terms = []
            | termsOfTokens (Parse.Ast.ConsToken (_, token, tokens)) terms =
                termsOfToken token (termsOfTokens tokens terms)
          and termsOfToken (Parse.Ast.Keyword (_, name, literal)) terms =
                let
                  val hand = (name, 0)
                  val symbol = (hand, UnitTerm)
                  val (term, present) = SymbolHashTable.lookupOrInsert' table hand (fn () => symbol)
                  val literalHand = (literal, ~1)
                in
                  (SymbolHashTable.lookupOrInsert table literalHand (fn () => symbol);
                  if present then terms else term::terms)
                end
            | termsOfToken (Parse.Ast.AttrToken (_, name, attr)) terms =
                let
                  val hand = (name, 0)
                  val kind =
                    case attr of
                      "string" => StrTerm
                    | "int"    => IntTerm
                    | "char"   => CharTerm
                    | "real"   => RealTerm
                    | t        => raise Fail ("unknown type: " ^ t)
                  val symbol = (hand, kind)
                  val (term, present) = SymbolHashTable.lookupOrInsert' table hand (fn () => symbol)
                in
                  if present then terms else term::terms
                end
            | termsOfToken (Parse.Ast.NoAttrToken (_, name)) terms =
                let
                  val hand = (name, 0)
                  val symbol = (hand, UnitTerm)
                  val (term, present) = SymbolHashTable.lookupOrInsert' table hand (fn () => symbol)
                in
                  if present then terms else term::terms
                end
        in
          termsOfGrammar ast []
        end
      val nonterms =
        let
          fun nontermsOfGrammar (Parse.Ast.Grammar (_, tokens, defs)) syms =
                nontermsOfDefs defs syms
          and nontermsOfDefs (Parse.Ast.NilDef _) syms = []
            | nontermsOfDefs (Parse.Ast.ConsDef (_, def, defs)) syms =
                nontermsOfDef def (nontermsOfDefs defs syms)
          and nontermsOfDef (Parse.Ast.Rule (_, label, cat, items)) syms =
                nontermsOfCat cat syms
          and nontermsOfCat cat syms = 
                let
                  val hand = catToHandle cat
                  fun symf () = (hand, Nonterm)
                  val (sym, present) = SymbolHashTable.lookupOrInsert' table hand symf
                in
                  if present then syms else sym::syms
                end
        in
          nontermsOfGrammar ast []
        end
      (* val entries = SymbolHashTable.toList table
      val _ = List.app (fn ((name, level),v) => print (name ^ ":" ^ Int.toString level ^ "\n")) entries *)
      val rules =
        let
          fun rulesOfGrammar (Parse.Ast.Grammar (_, terminals, defs)) rules = rulesOfDefs defs rules
          and rulesOfDefs (Parse.Ast.NilDef _) rules = []
            | rulesOfDefs (Parse.Ast.ConsDef (_, def, defs)) rules =
                rulesOfDef def (rulesOfDefs defs rules)
          and rulesOfDef (Parse.Ast.Rule (_, label, cat, items)) rules =
                let
                  fun toList (Parse.Ast.NilItem _) = []
                    | toList (Parse.Ast.ConsItem (_, item, items)) = item::(toList items)
                  val items' = toList items
                  val cons = labelToCons label
                  val lhs = SymbolHashTable.lookup table (catToHandle cat) handle Absent => raise Fail "b"
                  fun l i =
                    let
                      val h = itemToHandle i
                    in
                      SymbolHashTable.lookup table h
                      handle Absent => raise Fail (#1 h)
                    end
                  val rhs = map l items'
                in
                  (cons, lhs, rhs)::rules
                end
        in
          rulesOfGrammar ast []
        end
      val start = lhsOf (hd rules)
    in
      {terms = terms, nonterms = nonterms, rules = rules, start = start}
    end

  fun makeGrammar terms nonterms rules startSymbol =
    let
      val rules = List.map makeRule rules
    in
      (terms, nonterms, rules, startSymbol)
    end

  fun startSymbolOf ({start,...} : grammar) = start
  fun termsOf ({terms,...} : grammar) = terms
  fun nontermsOf ({nonterms,...} : grammar) = nonterms

  fun isConsDefined Wild = false | isConsDefined _ = true
  fun showCons (Id s) = s
    | showCons Wild = "_"
    | showCons ListE = "[]"
    | showCons ListCons = "(:)"
    | showCons ListOne = "(:[])"
  fun showRule (con, lhs, rhs) =
    showCons con ^ ". "
      ^ showSymbol lhs ^ " ::= "
      ^ String.concatWith " " (List.map showSymbol rhs) ^ ";"
  fun printGrammar ({rules,...} : grammar) =
    let
      fun printRule rule = print (showRule rule ^ "\n")
    in
      List.app printRule rules
    end
end

signature LRITEM = sig
  eqtype item
  type items = item list
  val fromRule : Grammar.rule -> item
  val expand : items -> Grammar.rule list -> items
  val moveOver : items -> Grammar.symbol -> Grammar.rule list -> items
  val nextSymbols : items -> Grammar.symbol list
  val partition :items -> items * items
  val consOf : item -> Grammar.constructor
  val lhsOf : item -> Grammar.symbol
  val rhsBeforeDot : item -> Grammar.symbol list
  val show : item -> string
end

structure LrItem :> LRITEM = struct
  local
    open Grammar
    type lhs = symbol
    type rhs_before_dot = symbol list
    type rhs_after_dot = symbol list
  in
    type item = constructor * lhs * rhs_before_dot * rhs_after_dot
    type items = item list
  end

  fun fromRule rule = (Grammar.consOf rule, Grammar.lhsOf rule, [], Grammar.rhsOf rule)

  fun expand (lrItems : items) (rules : Grammar.rule list) =
    let
      (* 1st = unexpanded items, 2nd = expanded items *)
      fun loop [] expanded = expanded
        | loop (lrItem::lrItems) expanded =
            if Util.mem lrItem expanded then loop lrItems expanded
            else
              case lrItem of
                (* if the dot is not in fromt of a non-terminal *)
                (_, _, _, [])     => loop lrItems (lrItem::expanded)
              | (_, _, _, sym::_) =>
                  if Grammar.isTerm sym then
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
      fun move (c, n, l, []) = NONE
        | move (c, n, l, next::rest) =
          if next = symbol then SOME (c, n, l @ [next], rest)
          else NONE
      val moved = List.mapPartial move items
    in
      expand moved grammar
    end

  fun endsWithDot (_, _, _, []) = true
    | endsWithDot _ = false

  fun nextSymbols lrItems =
    let
      fun loop [] symbols = symbols
        | loop ((_, _, _, [])::lrItems) symbols = loop lrItems symbols
        | loop ((_, _, _, nextSymbol::_)::lrItems) symbols =
            loop lrItems (Util.add (nextSymbol, symbols))
    in
      loop lrItems []
    end

  fun partition lrItems =
    List.partition endsWithDot lrItems

  fun consOf (cons, _, _, _) = cons
  fun lhsOf (_, lhs, _, _) = lhs
  fun rhsBeforeDot (_, _, rhs, []) = rhs

  fun show (_, lhs, rhs1, rhs2) =
    Grammar.showSymbol lhs ^ " -> "
      ^ String.concatWith " " (List.map Grammar.showSymbol rhs1)
      ^ " . "
      ^ String.concatWith " " (List.map Grammar.showSymbol rhs2)
end

structure State = struct
  type state = LrItem.items * LrItem.items
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
  val printAutomaton : automaton -> unit
end

structure Automaton :> AUTOMATON where
  type state = State.state
  and type alphabet = Grammar.symbol
  = struct
  open State
  type state = State.state
  type state_number = int
  type alphabet = Grammar.symbol
  type transition = state_number * alphabet * state_number
  type automaton = LrItem.items Intern.pool * transition list

  fun stateOfLrItems lrItems = LrItem.partition lrItems

  fun makeAutomaton grammar =
    let
      val startRule = Grammar.makeRule (Grammar.Wild , Grammar.S', [Grammar.startSymbolOf grammar])
      val rules = Grammar.rulesOf grammar
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

  fun printStates states =
    let
      fun showState state = String.concatWith " | " (List.map LrItem.show state)
      fun printState (n, state) = print (Int.toString n ^ ": " ^ showState state ^ "\n")
    in
      List.app printState (Intern.toList states)
    end
  
  fun printTransitions transitions =
    let
      fun printTransition (s1, symbol, s2) =
        print (Int.toString s1 ^ " -> " ^ Grammar.showSymbol symbol ^ " -> " ^ Int.toString s2 ^ "\n")
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
          handle BlockExp =>
            (out outs indent (pre ^ showPat pat ^ " =>");
            printExp outs (indent + 2) exp)
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

  fun kindToTycon Grammar.UnitTerm = NONE
    | kindToTycon Grammar.IntTerm  = SOME (MLAst.Tycon "int")
    | kindToTycon Grammar.StrTerm  = SOME (MLAst.Tycon "string")
    | kindToTycon Grammar.CharTerm = SOME (MLAst.Tycon "char")
    | kindToTycon Grammar.RealTerm = SOME (MLAst.Tycon "real")
    (* | kindToTycon Grammar.Nonterm  = raise Fail "nonterm has no attribute" *)

  (* string -> Grammar.symbol list -> MLAst.dec *)
  (* example output: datatype token = EOF | ... *)
  fun makeTokenDatatype typeName tokens =
    let
      fun f symbol = (Grammar.identOfSymbol symbol, kindToTycon (Grammar.kindOf symbol))
      fun makeTycons tokens = List.map f tokens
    in
      MLAst.Datatype [(typeName, makeTycons tokens)]
    end

  (* symbol list -> MLAst.dec *)
  (* example outpu: fun show (EOF) = "EOF" | ... *)
  fun makeShowFun tokens =
    let
      fun makePat symbol = 
        case Grammar.kindOf symbol of
          Grammar.Nonterm =>  MLAst.AsisPat ("(" ^ Grammar.identOfSymbol symbol ^ " _)")
        | Grammar.UnitTerm => MLAst.AsisPat ("(" ^ Grammar.identOfSymbol symbol ^ ")")
        | _ =>                MLAst.AsisPat ("(" ^ Grammar.identOfSymbol symbol ^ " a)")
      fun makeBody symbol =
        case Grammar.kindOf symbol of
          Grammar.UnitTerm => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "\"")
        | Grammar.IntTerm  => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "(\" ^ Int.toString a ^ \")\"")
        | Grammar.StrTerm  => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "(\" ^ a ^ \")\"")
        | Grammar.CharTerm => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "(\" ^ Char.toString a ^ \")\"")
        | Grammar.RealTerm => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "(\" ^ Real.toString a ^ \")\"")
        | Grammar.Nonterm => MLAst.AsisExp ("\"" ^ Grammar.identOfSymbol symbol ^ "\"")
      val patExps = List.map (fn token => ([makePat token], makeBody token)) tokens
    in
      MLAst.Fun [("show", patExps)]
    end

  fun nt2dt nonterm = Util.toLower (Util.chopDigit (Grammar.identOfSymbol nonterm))

  (* makeAstDatatype : string list -> Grammar.rule list -> MLAst.dec *)
  (* example output:
       datatype grammar =
         Grammar of Lex.span * defs
       and defs = ... *)
  fun makeAstDatatype datatypeNames rules =
    let
      fun makeDatatype name =
        let
          fun ruleFor name = List.filter (fn rule => name = nt2dt (Grammar.lhsOf rule) andalso Grammar.isConsDefined (Grammar.consOf rule)) rules
          fun symToTycon sym = 
            case Grammar.kindOf sym of
              Grammar.Nonterm => SOME (MLAst.Tycon (nt2dt sym))
            | kind => kindToTycon kind
          fun f rhs =
            case List.mapPartial symToTycon rhs of
              [] => MLAst.Tycon "Lex.span"
            | tys => MLAst.TupleType (MLAst.Tycon "Lex.span"::tys)
          fun consId (Grammar.Id l) = l
        in
          (* type name and constructors *)
          (name,
          List.map (fn rule => (consId (Grammar.consOf rule), SOME (f (Grammar.rhsOf rule)))) (ruleFor name))
        end
    in
      (* this makes mutually recursive datatypes *)
      MLAst.Datatype (List.map makeDatatype datatypeNames)
    end

  (* example output:
       datatype category =
         EOF
       | Grammar of Ast.grammar
       ... *)
  fun makeCategoryDatatype typeName symbols =
    let
      fun f symbol =
        let val name = Grammar.identOfSymbol symbol in
          case Grammar.kindOf symbol of
            Grammar.Nonterm => (name, SOME (MLAst.Tycon ("Ast." ^ nt2dt symbol)))
          | kind => (name, kindToTycon kind)
        end
      fun makeTycons tokens = List.map f tokens
    in
      MLAst.Datatype [(typeName, makeTycons symbols)]
    end

  (* example output:
       fun fromToken (Token.EOF) = EOF
         | fromToken ... *)
  fun makeFromTokenFun tokens =
    let
      fun makePat symbol =
        case Grammar.kindOf symbol of
          Grammar.Nonterm  => raise Fail ""
        | Grammar.UnitTerm => MLAst.AsisPat ("(Token." ^ Grammar.identOfSymbol symbol ^ ")")
        | _                => MLAst.AsisPat ("(Token." ^ Grammar.identOfSymbol symbol ^ " a)")
      fun makeBody symbol =
        case Grammar.kindOf symbol of
          Grammar.Nonterm  => raise Fail ""
        | Grammar.UnitTerm => MLAst.AsisExp (Grammar.identOfSymbol symbol)
        | _                => MLAst.AsisExp (Grammar.identOfSymbol symbol ^ " a")
      val patExps = List.map (fn token => ([makePat token], makeBody token)) tokens
    in
      MLAst.Fun [("fromToken", patExps)]
    end

  fun holdSv sym =
    case Grammar.kindOf sym of
      Grammar.UnitTerm => false
    | _ => true

  (* st functions *)
  fun makeStMrule automaton (symbol, next) =
    let
      val pat = if holdSv symbol
                then MLAst.AsisPat (Grammar.identOfSymbol symbol ^ " _")
                else MLAst.AsisPat (Grammar.identOfSymbol symbol)
      val (reduce, shift) = Automaton.stateOf next automaton
      val stNum = Int.toString next
      val shiftExp =
        if shift = [] then "[]"
        else "[(" ^ stNum ^ ", (stackItem::stack))]" 
      val reduceExp =
        if reduce = [] then ""
        else " @ st" ^ stNum ^ "r (stackItem::stack) toPos"
      val exp = MLAst.AsisExp (shiftExp ^ reduceExp)
    in
      (pat, exp)
    end

  fun makeStFvalbind automaton stateNumber =
    let
      val n = Int.toString stateNumber
      val (reduce, shift) = Automaton.stateOf stateNumber automaton
      fun stReduce item = 
        let
          val cons = LrItem.consOf item
          val lhs = LrItem.lhsOf item
          val rhs = LrItem.rhsBeforeDot item
          val isYpsilon = length rhs = 0
          val fromPos = if isYpsilon then "pos" else "pos0"
          val index = ref 0
          val stackPat =
            List.foldl
            (fn (sym, pats) =>
              let
                val n = Int.toString (!index)
                val sv = if holdSv sym then SOME ("sv" ^ n) else NONE
              in
                index := !index + 1;
                (sym, sv, "stNum" ^ n, "pos" ^ n)::pats
              end)
            []
            rhs
          val stackPatString =
            let
              fun toString (sym, sv, stNum, pos) =
                "("
                ^ Grammar.identOfSymbol sym
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
              Grammar.Id c => MLAst.AppExp (MLAst.AsisExp ("Ast." ^ c), MLAst.TupleExp (MLAst.AsisExp ("(" ^ fromPos ^ ", pos)")::map MLAst.AsisExp svalues))
            | Grammar.Wild => MLAst.TupleExp (map MLAst.AsisExp svalues)
          val currentAst = MLAst.AppExp (MLAst.AsisExp (Grammar.identOfSymbol lhs), svaluesAst)
        in
          ("st" ^ n ^ "r", [
              if lhs = Grammar.S' then
                (map MLAst.AsisPat ["stack", "pos"],
                 MLAst.AsisExp "[(~1, stack)]")
              else
                (map MLAst.AsisPat ["(" ^ stackPatString ^ "stack)", "pos"],
                if isYpsilon then
                  MLAst.AsisExp ("go " ^ n ^ " stack " ^ MLAst.showExp currentAst ^ " (pos, pos)")
                else
                  MLAst.AsisExp ("go stNum0 stack " ^ MLAst.showExp currentAst ^ " (pos0, pos)"))
          ])
        end
      val st = 
        let
          val nextStates = Automaton.nextStatesOf stateNumber automaton
          val lastMrule = (MLAst.AsisPat "c", MLAst.AsisExp ("[] (* raise Parse (c, pos, " ^ n ^ ") *)"))
          val stMrules = List.map (makeStMrule automaton) nextStates @ [lastMrule]
        in
          ("st" ^ n, [
            (map MLAst.AsisPat ["stack", "category", "(fromPos, toPos)"],
            MLAst.Let ([
              MLAst.AsisDec ("val stackItem = (category, fromPos, " ^ n ^ ")")],
              MLAst.Case (MLAst.AsisExp "category", stMrules)))
          ])
        end
    in
      (if shift = [] then [] else [st]) @ map stReduce reduce
    end

  fun generateParser outs grammar =
    let
      val tokens = Grammar.EOF :: Grammar.termsOf grammar
      val nonterms = Grammar.nontermsOf grammar
      val rules = Grammar.rulesOf grammar
      val categories = tokens @ nonterms
      val automaton = Automaton.makeAutomaton grammar
      val numbersAndStates = Automaton.numbersAndStates automaton
      val stateNumbers = List.map #1 numbersAndStates
    
      (* Token *)
      val tokenDatatype = makeTokenDatatype "token" tokens
      val tokenShowFun = makeShowFun tokens
      val tokenStructure =
        MLAst.Structure [("Token", MLAst.Struct
          [MLAst.Dec tokenDatatype,
           MLAst.Dec tokenShowFun])]
    
      (* Aat *)
      val astDatatypeNames = List.foldr Util.add [] (map nt2dt nonterms)
      val astDatatype = makeAstDatatype astDatatypeNames rules
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
      val nonacceptingStateNumbers = List.filter (fn number => let val (_, s) = Automaton.stateOf number automaton in s <> [] end) stateNumbers
      val stateNumbersAsString = List.map Int.toString nonacceptingStateNumbers
      val goMrules = List.map (fn n => (MLAst.AsisPat n, MLAst.AsisExp ("st" ^ n ^ " stack category span"))) stateNumbersAsString
      val goCase = MLAst.Case (MLAst.AsisExp "stateNumber", goMrules @ [(MLAst.AsisPat "_", MLAst.AsisExp "[]")])
      val goFvalbind =
        ("go", [
          (map MLAst.AsisPat ["stateNumber", "stack", "category", "span"], goCase)
        ])
    
      val st = List.concat (List.map (makeStFvalbind automaton) (Automaton.stateNumbers automaton))
      (* state machine function *)
      val stFuns = MLAst.Fun (goFvalbind::st)
    
      val lexSignature = MLAst.Signature [("Lex",
        MLAst.Sig [
          MLAst.TypeSpec ["strm"],
          MLAst.TypeSpec ["pos"],
          MLAst.TypeSpec ["span = pos * pos"], (* dirty *)
          MLAst.TypeSpec ["tok"],
          MLAst.ValSpec [("lex", MLAst.AsisType "AntlrStreamPos.sourcemap -> strm -> tok * span * strm")],
          MLAst.ValSpec [("getPos", MLAst.AsisType "strm -> pos")]])]

      val parseLoop = MLAst.Fun [
        ("loop" , [([MLAst.AsisPat "stacks", MLAst.AsisPat "strm"],
          MLAst.Let (
            [MLAst.AsisDec "val pos = Lex.getPos strm",
             MLAst.AsisDec "val (token, span, strm') = Lex.lex sourcemap strm"],
            MLAst.Case (MLAst.AsisExp "token",
            [(MLAst.AsisPat "Token.EOF", MLAst.AsisExp "map (fn (st, stack) => stack) (List.filter (fn (st, _) => st = ~1) stacks)"),
             (MLAst.AsisPat "_",
               MLAst.Let (
                 [MLAst.AsisDec "val category = Category.fromToken token",
                  MLAst.AsisDec "val stacks' = List.concat (map (fn (st, stack) => go st stack category span) stacks)"],
                  MLAst.AsisExp "loop stacks' strm'"))
            ])))])]

      val reduceExp =
        let
          val (reduce, shift) = Automaton.stateOf 0 automaton
        in
          if reduce = [] then "" else " @ st0r [] pos"
        end
      val parseFun = MLAst.Fun [
        ("parse", [([MLAst.AsisPat "sourcemap", MLAst.AsisPat "strm"],
          MLAst.Let (
            [MLAst.AsisDec "val pos = Lex.getPos strm",
             MLAst.AsisDec ("val stacks = [(0, [])]" ^ reduceExp),
             parseLoop],
            MLAst.AsisExp "loop stacks strm"))])]
    
      val parseStructure = MLAst.Struct [
        astStructure,
        categoryStructure,
        MLAst.Dec (MLAst.AsisDec "open Category"),
        MLAst.Dec (MLAst.AsisDec "exception Parse of category * Lex.pos * int"),
        MLAst.Dec stFuns,
        MLAst.Dec parseFun
      ]
    
      val parseFunctor = MLAst.Functor [(
        "ParseFun",
        "Lex",
        MLAst.SigId ("Lex", [("tok", MLAst.Tycon "Token.token")]),
        parseStructure)]
    in
      MLAst.printStrdec outs 0 tokenStructure;
      MLAst.printSigdec outs 0 lexSignature;
      MLAst.printFundec outs 0 parseFunctor
    end
end

structure Main = struct
  fun main ins inFileName outs =
    let
      val strm = Lexer.streamifyInstream ins
      val sourcemap =
        case inFileName of 
          NONE => AntlrStreamPos.mkSourcemap ()
        | SOME name => AntlrStreamPos.mkSourcemap' name
(*    in
      Parse.parse sourcemap strm *)
      val [[(Parse.Category.Grammar ast, _, _)]] = Parse.parse sourcemap strm
      val grammar = Grammar.fromAst ast
    in
      CodeGenerator.generateParser outs grammar
    end
end

fun main () =
  Main.main TextIO.stdIn NONE TextIO.stdOut
