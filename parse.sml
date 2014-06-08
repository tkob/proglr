structure Token = struct
  datatype token =
    EOF
  | SEMI
  | DOT
  | AS
  | STRING of string
  | LBRACKET
  | RBRACKET
  | IDENT of string
  | UNDERSCORE
  | LPAREN
  | COLON
  | RPAREN
  | INT of int
  | FLOAT of real
  fun show (EOF) = "EOF"
    | show (SEMI) = "SEMI"
    | show (DOT) = "DOT"
    | show (AS) = "AS"
    | show (STRING a) = "STRING(" ^ a ^ ")"
    | show (LBRACKET) = "LBRACKET"
    | show (RBRACKET) = "RBRACKET"
    | show (IDENT a) = "IDENT(" ^ a ^ ")"
    | show (UNDERSCORE) = "UNDERSCORE"
    | show (LPAREN) = "LPAREN"
    | show (COLON) = "COLON"
    | show (RPAREN) = "RPAREN"
    | show (INT a) = "INT"
    | show (FLOAT a) = "FLOAT"
end
signature Lex = sig
  type strm
  type pos
  type span = pos * pos
  type tok
  val lex : AntlrStreamPos.sourcemap -> strm -> tok * span * strm
  val getPos : strm -> pos
end
functor ParseFun(Lex : Lex where type tok = Token.token) = struct
  structure Ast = struct
    datatype grammar =
      Grammar of Lex.span * defs
    and defs =
      NilDef of Lex.span
    | ConsDef of Lex.span * def * defs
    and items =
      NilItem of Lex.span
    | ConsItem of Lex.span * item * items
    and def =
      Rule of Lex.span * label * cat * items
    and item =
      Terminal of Lex.span * string
    | NTerminal of Lex.span * cat
    and cat =
      ListCat of Lex.span * cat
    | IdCat of Lex.span * string
    and label =
      Id of Lex.span * string
    | Wild of Lex.span
    | ListE of Lex.span
    | ListCons of Lex.span
    | ListOne of Lex.span
  end
  structure Category = struct
    datatype category =
      EOF
    | SEMI
    | DOT
    | AS
    | STRING of string
    | LBRACKET
    | RBRACKET
    | IDENT of string
    | UNDERSCORE
    | LPAREN
    | COLON
    | RPAREN
    | Grammar of Ast.grammar
    | Defs of Ast.defs
    | Items of Ast.items
    | Def of Ast.def
    | Item of Ast.item
    | Cat of Ast.cat
    | Label of Ast.label
    fun show (EOF) = "EOF"
      | show (SEMI) = "SEMI"
      | show (DOT) = "DOT"
      | show (AS) = "AS"
      | show (STRING a) = "STRING(" ^ a ^ ")"
      | show (LBRACKET) = "LBRACKET"
      | show (RBRACKET) = "RBRACKET"
      | show (IDENT a) = "IDENT(" ^ a ^ ")"
      | show (UNDERSCORE) = "UNDERSCORE"
      | show (LPAREN) = "LPAREN"
      | show (COLON) = "COLON"
      | show (RPAREN) = "RPAREN"
      | show (Grammar _) = "Grammar"
      | show (Defs _) = "Defs"
      | show (Items _) = "Items"
      | show (Def _) = "Def"
      | show (Item _) = "Item"
      | show (Cat _) = "Cat"
      | show (Label _) = "Label"
    fun fromToken (Token.EOF) = EOF
      | fromToken (Token.SEMI) = SEMI
      | fromToken (Token.DOT) = DOT
      | fromToken (Token.AS) = AS
      | fromToken (Token.STRING a) = STRING a
      | fromToken (Token.LBRACKET) = LBRACKET
      | fromToken (Token.RBRACKET) = RBRACKET
      | fromToken (Token.IDENT a) = IDENT a
      | fromToken (Token.UNDERSCORE) = UNDERSCORE
      | fromToken (Token.LPAREN) = LPAREN
      | fromToken (Token.COLON) = COLON
      | fromToken (Token.RPAREN) = RPAREN
  end
  open Category
  exception Parse of category * Lex.pos * int
  fun go stateNumber stack category span =
      case stateNumber of
        27 => st27 stack category span
      | 26 => st26 stack category span
      | 24 => st24 stack category span
      | 21 => st21 stack category span
      | 17 => st17 stack category span
      | 15 => st15 stack category span
      | 13 => st13 stack category span
      | 12 => st12 stack category span
      | 11 => st11 stack category span
      | 9 => st9 stack category span
      | 8 => st8 stack category span
      | 7 => st7 stack category span
      | 4 => st4 stack category span
      | 3 => st3 stack category span
      | 0 => st0 stack category span
      | _ => []
  and st28r ((RPAREN, pos4, stNum4)::(RBRACKET, pos3, stNum3)::(LBRACKET, pos2, stNum2)::(COLON, pos1, stNum1)::(LPAREN, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListOne ((pos0, pos)))) (pos0, pos)
  and st27 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 27)
      in
        case category of
          RPAREN => [] @ st28r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 27) *)
      end
  and st26 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 26)
      in
        case category of
          RBRACKET => [(27, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 26) *)
      end
  and st25r ((RPAREN, pos2, stNum2)::(COLON, pos1, stNum1)::(LPAREN, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListCons ((pos0, pos)))) (pos0, pos)
  and st24 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 24)
      in
        case category of
          RPAREN => [] @ st25r (stackItem::stack) toPos
        | LBRACKET => [(26, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 24) *)
      end
  and st23r ((RBRACKET, pos1, stNum1)::(LBRACKET, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListE ((pos0, pos)))) (pos0, pos)
  and st22r ((RBRACKET, pos2, stNum2)::(Cat sv1, pos1, stNum1)::(LBRACKET, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.ListCat ((pos0, pos), sv1))) (pos0, pos)
  and st21 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 21)
      in
        case category of
          RBRACKET => [] @ st22r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 21) *)
      end
  and st20r ((Items sv1, pos1, stNum1)::(Item sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Items (Ast.ConsItem ((pos0, pos), sv0, sv1))) (pos0, pos)
  and st19r ((Cat sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.NTerminal ((pos0, pos), sv0))) (pos0, pos)
  and st18r ((STRING sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.Terminal ((pos0, pos), sv0))) (pos0, pos)
  and st17 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 17)
      in
        case category of
          Items _ => [] @ st20r (stackItem::stack) toPos
        | Item _ => [(17, (stackItem::stack))] @ st17r (stackItem::stack) toPos
        | STRING _ => [] @ st18r (stackItem::stack) toPos
        | Cat _ => [] @ st19r (stackItem::stack) toPos
        | LBRACKET => [(13, (stackItem::stack))]
        | IDENT _ => [] @ st14r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 17) *)
      end
  and st17r (stack) pos =
      go 17 stack (Items (Ast.NilItem ((pos, pos)))) (pos, pos)
  and st16r ((Items sv4, pos4, stNum4)::(AS, pos3, stNum3)::(Cat sv2, pos2, stNum2)::(DOT, pos1, stNum1)::(Label sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Rule ((pos0, pos), sv0, sv2, sv4))) (pos0, pos)
  and st15 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 15)
      in
        case category of
          Items _ => [] @ st16r (stackItem::stack) toPos
        | Item _ => [(17, (stackItem::stack))] @ st17r (stackItem::stack) toPos
        | STRING _ => [] @ st18r (stackItem::stack) toPos
        | Cat _ => [] @ st19r (stackItem::stack) toPos
        | LBRACKET => [(13, (stackItem::stack))]
        | IDENT _ => [] @ st14r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 15) *)
      end
  and st15r (stack) pos =
      go 15 stack (Items (Ast.NilItem ((pos, pos)))) (pos, pos)
  and st14r ((IDENT sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.IdCat ((pos0, pos), sv0))) (pos0, pos)
  and st13 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 13)
      in
        case category of
          Cat _ => [(21, (stackItem::stack))]
        | LBRACKET => [(13, (stackItem::stack))]
        | IDENT _ => [] @ st14r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 13) *)
      end
  and st12 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 12)
      in
        case category of
          AS => [(15, (stackItem::stack))] @ st15r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 12) *)
      end
  and st11 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 11)
      in
        case category of
          Cat _ => [(12, (stackItem::stack))]
        | LBRACKET => [(13, (stackItem::stack))]
        | IDENT _ => [] @ st14r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 11) *)
      end
  and st10r ((Defs sv2, pos2, stNum2)::(SEMI, pos1, stNum1)::(Def sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Defs (Ast.ConsDef ((pos0, pos), sv0, sv2))) (pos0, pos)
  and st9 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 9)
      in
        case category of
          Defs _ => [] @ st10r (stackItem::stack) toPos
        | Def _ => [(3, (stackItem::stack))]
        | Label _ => [(4, (stackItem::stack))]
        | IDENT _ => [] @ st5r (stackItem::stack) toPos
        | UNDERSCORE => [] @ st6r (stackItem::stack) toPos
        | LBRACKET => [(7, (stackItem::stack))]
        | LPAREN => [(8, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 9) *)
      end
  and st9r (stack) pos =
      go 9 stack (Defs (Ast.NilDef ((pos, pos)))) (pos, pos)
  and st8 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 8)
      in
        case category of
          COLON => [(24, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 8) *)
      end
  and st7 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 7)
      in
        case category of
          RBRACKET => [] @ st23r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 7) *)
      end
  and st6r ((UNDERSCORE, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Wild ((pos0, pos)))) (pos0, pos)
  and st5r ((IDENT sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Id ((pos0, pos), sv0))) (pos0, pos)
  and st4 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 4)
      in
        case category of
          DOT => [(11, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 4) *)
      end
  and st3 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 3)
      in
        case category of
          SEMI => [(9, (stackItem::stack))] @ st9r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 3) *)
      end
  and st2r ((Defs sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Grammar (Ast.Grammar ((pos0, pos), sv0))) (pos0, pos)
  and st1r stack pos = [(~1, stack)]
  and st0 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 0)
      in
        case category of
          Grammar _ => [] @ st1r (stackItem::stack) toPos
        | Defs _ => [] @ st2r (stackItem::stack) toPos
        | Def _ => [(3, (stackItem::stack))]
        | Label _ => [(4, (stackItem::stack))]
        | IDENT _ => [] @ st5r (stackItem::stack) toPos
        | UNDERSCORE => [] @ st6r (stackItem::stack) toPos
        | LBRACKET => [(7, (stackItem::stack))]
        | LPAREN => [(8, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 0) *)
      end
  and st0r (stack) pos =
      go 0 stack (Defs (Ast.NilDef ((pos, pos)))) (pos, pos)
  fun parse sourcemap strm =
      let
        val pos = Lex.getPos strm
        val stacks = [(0, [])] @ st0r [] pos
        fun loop stacks strm =
            let
              val pos = Lex.getPos strm
              val (token, span, strm') = Lex.lex sourcemap strm
            in
              case token of
                Token.EOF => map (fn (st, stack) => stack) (List.filter (fn (st, _) => st = ~1) stacks)
              | _ =>
                let
                  val category = Category.fromToken token
                  val stacks' = List.concat (map (fn (st, stack) => go st stack category span) stacks)
                in
                  loop stacks' strm'
                end
            end
      in
        loop stacks strm
      end
end

