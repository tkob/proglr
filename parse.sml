structure Token = struct
  datatype token =
    EOF
  | Semi
  | Dot
  | As
  | LBracket
  | RBracket
  | Underscore
  | LParen
  | RParen
  | Colon
  | TokenKw
  | OfKw
  | Integer of int
  | Double of real
  | Char of char
  | String of string
  | Ident of string
  fun show (EOF) = "EOF"
    | show (Semi) = "Semi"
    | show (Dot) = "Dot"
    | show (As) = "As"
    | show (LBracket) = "LBracket"
    | show (RBracket) = "RBracket"
    | show (Underscore) = "Underscore"
    | show (LParen) = "LParen"
    | show (RParen) = "RParen"
    | show (Colon) = "Colon"
    | show (TokenKw) = "TokenKw"
    | show (OfKw) = "OfKw"
    | show (Integer a) = "Integer(" ^ Int.toString a ^ ")"
    | show (Double a) = "Double(" ^ Real.toString a ^ ")"
    | show (Char a) = "Char(" ^ Char.toString a ^ ")"
    | show (String a) = "String(" ^ a ^ ")"
    | show (Ident a) = "Ident(" ^ a ^ ")"
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
      Grammar of Lex.span * token list * def list
    and token =
      Keyword of Lex.span * string * string
    | AttrToken of Lex.span * string * string
    | NoAttrToken of Lex.span * string
    and def =
      Rule of Lex.span * label * cat * item list
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
    | Semi
    | Dot
    | As
    | LBracket
    | RBracket
    | Underscore
    | LParen
    | RParen
    | Colon
    | TokenKw
    | OfKw
    | Integer of int
    | Double of real
    | Char of char
    | String of string
    | Ident of string
    | Grammar of Ast.grammar
    | Token' of Ast.token list
    | Token of Ast.token
    | Def' of Ast.def list
    | Item' of Ast.item list
    | Def of Ast.def
    | Item of Ast.item
    | Cat of Ast.cat
    | Label of Ast.label
    fun show (EOF) = "EOF"
      | show (Semi) = "Semi"
      | show (Dot) = "Dot"
      | show (As) = "As"
      | show (LBracket) = "LBracket"
      | show (RBracket) = "RBracket"
      | show (Underscore) = "Underscore"
      | show (LParen) = "LParen"
      | show (RParen) = "RParen"
      | show (Colon) = "Colon"
      | show (TokenKw) = "TokenKw"
      | show (OfKw) = "OfKw"
      | show (Integer a) = "Integer(" ^ Int.toString a ^ ")"
      | show (Double a) = "Double(" ^ Real.toString a ^ ")"
      | show (Char a) = "Char(" ^ Char.toString a ^ ")"
      | show (String a) = "String(" ^ a ^ ")"
      | show (Ident a) = "Ident(" ^ a ^ ")"
      | show (Grammar _) = "Grammar"
      | show (Token' _) = "Token'"
      | show (Token _) = "Token"
      | show (Def' _) = "Def'"
      | show (Item' _) = "Item'"
      | show (Def _) = "Def"
      | show (Item _) = "Item"
      | show (Cat _) = "Cat"
      | show (Label _) = "Label"
    fun fromToken (Token.EOF) = EOF
      | fromToken (Token.Semi) = Semi
      | fromToken (Token.Dot) = Dot
      | fromToken (Token.As) = As
      | fromToken (Token.LBracket) = LBracket
      | fromToken (Token.RBracket) = RBracket
      | fromToken (Token.Underscore) = Underscore
      | fromToken (Token.LParen) = LParen
      | fromToken (Token.RParen) = RParen
      | fromToken (Token.Colon) = Colon
      | fromToken (Token.TokenKw) = TokenKw
      | fromToken (Token.OfKw) = OfKw
      | fromToken (Token.Integer a) = Integer a
      | fromToken (Token.Double a) = Double a
      | fromToken (Token.Char a) = Char a
      | fromToken (Token.String a) = String a
      | fromToken (Token.Ident a) = Ident a
  end
  open Category
  exception Parse of category * Lex.pos * int
  fun go stateNumber stack category span =
      case stateNumber of
        36 => st36 stack category span
      | 34 => st34 stack category span
      | 32 => st32 stack category span
      | 30 => st30 stack category span
      | 29 => st29 stack category span
      | 27 => st27 stack category span
      | 24 => st24 stack category span
      | 20 => st20 stack category span
      | 18 => st18 stack category span
      | 16 => st16 stack category span
      | 15 => st15 stack category span
      | 14 => st14 stack category span
      | 12 => st12 stack category span
      | 11 => st11 stack category span
      | 10 => st10 stack category span
      | 7 => st7 stack category span
      | 6 => st6 stack category span
      | 4 => st4 stack category span
      | 3 => st3 stack category span
      | 2 => st2 stack category span
      | 0 => st0 stack category span
      | _ => []
  and st37r ((Ident sv3, pos3, stNum3)::(OfKw, pos2, stNum2)::(Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.AttrToken ((pos0, pos), sv1, sv3))) (pos0, pos)
  and st36 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 36)
      in
        case category of
          Ident _ => [] @ st37r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 36) *)
      end
  and st35r ((String sv2, pos2, stNum2)::(Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.Keyword ((pos0, pos), sv1, sv2))) (pos0, pos)
  and st34 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 34)
      in
        case category of
          String _ => [] @ st35r (stackItem::stack) toPos
        | OfKw => [(36, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 34) *)
      end
  and st34r ((Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.NoAttrToken ((pos0, pos), sv1))) (pos0, pos)
  and st33r ((Token' sv2, pos2, stNum2)::(Semi, pos1, stNum1)::(Token sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token' (sv0::sv2)) (pos0, pos)
  and st32 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 32)
      in
        case category of
          Token' _ => [] @ st33r (stackItem::stack) toPos
        | Token _ => [(3, (stackItem::stack))]
        | TokenKw => [(4, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 32) *)
      end
  and st32r (stack) pos = go 32 stack (Token' []) (pos, pos)
  and st31r ((RParen, pos4, stNum4)::(RBracket, pos3, stNum3)::(LBracket, pos2, stNum2)::(Colon, pos1, stNum1)::(LParen, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListOne ((pos0, pos)))) (pos0, pos)
  and st30 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 30)
      in
        case category of
          RParen => [] @ st31r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 30) *)
      end
  and st29 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 29)
      in
        case category of
          RBracket => [(30, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 29) *)
      end
  and st28r ((RParen, pos2, stNum2)::(Colon, pos1, stNum1)::(LParen, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListCons ((pos0, pos)))) (pos0, pos)
  and st27 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 27)
      in
        case category of
          RParen => [] @ st28r (stackItem::stack) toPos
        | LBracket => [(29, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 27) *)
      end
  and st26r ((RBracket, pos1, stNum1)::(LBracket, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListE ((pos0, pos)))) (pos0, pos)
  and st25r ((RBracket, pos2, stNum2)::(Cat sv1, pos1, stNum1)::(LBracket, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.ListCat ((pos0, pos), sv1))) (pos0, pos)
  and st24 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 24)
      in
        case category of
          RBracket => [] @ st25r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 24) *)
      end
  and st23r ((Item' sv1, pos1, stNum1)::(Item sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item' (sv0::sv1)) (pos0, pos)
  and st22r ((Cat sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.NTerminal ((pos0, pos), sv0))) (pos0, pos)
  and st21r ((String sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.Terminal ((pos0, pos), sv0))) (pos0, pos)
  and st20 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 20)
      in
        case category of
          Item' _ => [] @ st23r (stackItem::stack) toPos
        | Item _ => [(20, (stackItem::stack))] @ st20r (stackItem::stack) toPos
        | String _ => [] @ st21r (stackItem::stack) toPos
        | Cat _ => [] @ st22r (stackItem::stack) toPos
        | LBracket => [(16, (stackItem::stack))]
        | Ident _ => [] @ st17r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 20) *)
      end
  and st20r (stack) pos = go 20 stack (Item' []) (pos, pos)
  and st19r ((Item' sv4, pos4, stNum4)::(As, pos3, stNum3)::(Cat sv2, pos2, stNum2)::(Dot, pos1, stNum1)::(Label sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Rule ((pos0, pos), sv0, sv2, sv4))) (pos0, pos)
  and st18 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 18)
      in
        case category of
          Item' _ => [] @ st19r (stackItem::stack) toPos
        | Item _ => [(20, (stackItem::stack))] @ st20r (stackItem::stack) toPos
        | String _ => [] @ st21r (stackItem::stack) toPos
        | Cat _ => [] @ st22r (stackItem::stack) toPos
        | LBracket => [(16, (stackItem::stack))]
        | Ident _ => [] @ st17r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 18) *)
      end
  and st18r (stack) pos = go 18 stack (Item' []) (pos, pos)
  and st17r ((Ident sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.IdCat ((pos0, pos), sv0))) (pos0, pos)
  and st16 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 16)
      in
        case category of
          Cat _ => [(24, (stackItem::stack))]
        | LBracket => [(16, (stackItem::stack))]
        | Ident _ => [] @ st17r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 16) *)
      end
  and st15 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 15)
      in
        case category of
          As => [(18, (stackItem::stack))] @ st18r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 15) *)
      end
  and st14 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 14)
      in
        case category of
          Cat _ => [(15, (stackItem::stack))]
        | LBracket => [(16, (stackItem::stack))]
        | Ident _ => [] @ st17r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 14) *)
      end
  and st13r ((Def' sv2, pos2, stNum2)::(Semi, pos1, stNum1)::(Def sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def' (sv0::sv2)) (pos0, pos)
  and st12 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 12)
      in
        case category of
          Def' _ => [] @ st13r (stackItem::stack) toPos
        | Def _ => [(6, (stackItem::stack))]
        | Label _ => [(7, (stackItem::stack))]
        | Ident _ => [] @ st8r (stackItem::stack) toPos
        | Underscore => [] @ st9r (stackItem::stack) toPos
        | LBracket => [(10, (stackItem::stack))]
        | LParen => [(11, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 12) *)
      end
  and st12r (stack) pos = go 12 stack (Def' []) (pos, pos)
  and st11 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 11)
      in
        case category of
          Colon => [(27, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 11) *)
      end
  and st10 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 10)
      in
        case category of
          RBracket => [] @ st26r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 10) *)
      end
  and st9r ((Underscore, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Wild ((pos0, pos)))) (pos0, pos)
  and st8r ((Ident sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Id ((pos0, pos), sv0))) (pos0, pos)
  and st7 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 7)
      in
        case category of
          Dot => [(14, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 7) *)
      end
  and st6 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 6)
      in
        case category of
          Semi => [(12, (stackItem::stack))] @ st12r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 6) *)
      end
  and st5r ((Def' sv1, pos1, stNum1)::(Token' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Grammar (Ast.Grammar ((pos0, pos), sv0, sv1))) (pos0, pos)
  and st4 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 4)
      in
        case category of
          Ident _ => [(34, (stackItem::stack))] @ st34r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 4) *)
      end
  and st3 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 3)
      in
        case category of
          Semi => [(32, (stackItem::stack))] @ st32r (stackItem::stack) toPos
        | c => [] (* raise Parse (c, pos, 3) *)
      end
  and st2 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 2)
      in
        case category of
          Def' _ => [] @ st5r (stackItem::stack) toPos
        | Def _ => [(6, (stackItem::stack))]
        | Label _ => [(7, (stackItem::stack))]
        | Ident _ => [] @ st8r (stackItem::stack) toPos
        | Underscore => [] @ st9r (stackItem::stack) toPos
        | LBracket => [(10, (stackItem::stack))]
        | LParen => [(11, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 2) *)
      end
  and st2r (stack) pos = go 2 stack (Def' []) (pos, pos)
  and st1r stack pos = [(~1, stack)]
  and st0 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 0)
      in
        case category of
          Grammar _ => [] @ st1r (stackItem::stack) toPos
        | Token' _ => [(2, (stackItem::stack))] @ st2r (stackItem::stack) toPos
        | Token _ => [(3, (stackItem::stack))]
        | TokenKw => [(4, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 0) *)
      end
  and st0r (stack) pos = go 0 stack (Token' []) (pos, pos)
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
