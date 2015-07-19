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
  | CommentKw
  | SeparatorKw
  | TerminatorKw
  | NonemptyKw
  | CoercionsKw
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
    | show (CommentKw) = "CommentKw"
    | show (SeparatorKw) = "SeparatorKw"
    | show (TerminatorKw) = "TerminatorKw"
    | show (NonemptyKw) = "NonemptyKw"
    | show (CoercionsKw) = "CoercionsKw"
end
signature Lex = sig
  type strm
  eqtype pos
  type span = pos * pos
  type tok
  val lex : AntlrStreamPos.sourcemap -> strm -> tok * span * strm
  val getPos : strm -> pos
end
functor ParseFun(Lex : Lex where type tok = Token.token and type pos = AntlrStreamPos.pos) = struct
  structure Ast = struct
    datatype grammar =
      Grammar of Lex.span * token list * def list
    and token =
      Keyword of Lex.span * string * string
    | AttrToken of Lex.span * string * string
    | NoAttrToken of Lex.span * string
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
    and def =
      Rule of Lex.span * label * cat * item list
    | Comment of Lex.span * string
    | Comments of Lex.span * string * string
    | Separator of Lex.span * minimumsize * cat * string
    | Terminator of Lex.span * minimumsize * cat * string
    | Coercions of Lex.span * string * int
    and minimumsize =
      MNonempty of Lex.span
    | MEmpty of Lex.span
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
    | CommentKw
    | SeparatorKw
    | TerminatorKw
    | NonemptyKw
    | CoercionsKw
    | Grammar of Ast.grammar
    | Token' of Ast.token list
    | Token of Ast.token
    | Def' of Ast.def list
    | Item' of Ast.item list
    | Item of Ast.item
    | Cat of Ast.cat
    | Label of Ast.label
    | Def of Ast.def
    | MinimumSize of Ast.minimumsize
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
      | show (CommentKw) = "CommentKw"
      | show (SeparatorKw) = "SeparatorKw"
      | show (TerminatorKw) = "TerminatorKw"
      | show (NonemptyKw) = "NonemptyKw"
      | show (CoercionsKw) = "CoercionsKw"
      | show (Grammar _) = "Grammar"
      | show (Token' _) = "Token'"
      | show (Token _) = "Token"
      | show (Def' _) = "Def'"
      | show (Item' _) = "Item'"
      | show (Item _) = "Item"
      | show (Cat _) = "Cat"
      | show (Label _) = "Label"
      | show (Def _) = "Def"
      | show (MinimumSize _) = "MinimumSize"
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
      | fromToken (Token.CommentKw) = CommentKw
      | fromToken (Token.SeparatorKw) = SeparatorKw
      | fromToken (Token.TerminatorKw) = TerminatorKw
      | fromToken (Token.NonemptyKw) = NonemptyKw
      | fromToken (Token.CoercionsKw) = CoercionsKw
  end
  open Category
  exception Parse of category * Lex.pos * int
  fun go stateNumber stack category span =
      case stateNumber of
        51 => st51 stack category span
      | 49 => st49 stack category span
      | 47 => st47 stack category span
      | 45 => st45 stack category span
      | 43 => st43 stack category span
      | 42 => st42 stack category span
      | 40 => st40 stack category span
      | 38 => st38 stack category span
      | 36 => st36 stack category span
      | 34 => st34 stack category span
      | 33 => st33 stack category span
      | 31 => st31 stack category span
      | 28 => st28 stack category span
      | 24 => st24 stack category span
      | 22 => st22 stack category span
      | 20 => st20 stack category span
      | 19 => st19 stack category span
      | 18 => st18 stack category span
      | 16 => st16 stack category span
      | 15 => st15 stack category span
      | 14 => st14 stack category span
      | 13 => st13 stack category span
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
  and st52r0 ((Ident sv3, pos3, stNum3)::(OfKw, pos2, stNum2)::(Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.AttrToken ((pos0, pos), sv1, sv3))) (pos0, pos)
    | st52r0 stack pos = []
  and st51 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 51)
      in
        case category of
          Ident _ => [] @ List.concat [st52r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 51) *)
      end
  and st50r0 ((String sv2, pos2, stNum2)::(Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.Keyword ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st50r0 stack pos = []
  and st49 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 49)
      in
        case category of
          String _ => [] @ List.concat [st50r0 (stackItem::stack) toPos]
        | OfKw => [(51, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 49) *)
      end
  and st49r0 ((Ident sv1, pos1, stNum1)::(TokenKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token (Ast.NoAttrToken ((pos0, pos), sv1))) (pos0, pos)
    | st49r0 stack pos = []
  and st48r0 ((Token' sv2, pos2, stNum2)::(Semi, pos1, stNum1)::(Token sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Token' (sv0::sv2)) (pos0, pos)
    | st48r0 stack pos = []
  and st47 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 47)
      in
        case category of
          Token' _ => [] @ List.concat [st48r0 (stackItem::stack) toPos]
        | Token _ => [(3, (stackItem::stack))]
        | TokenKw => [(4, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 47) *)
      end
  and st47r0 stack pos = go 47 stack (Token' []) (pos, pos)
  and st46r0 ((Integer sv2, pos2, stNum2)::(Ident sv1, pos1, stNum1)::(CoercionsKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Coercions ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st46r0 stack pos = []
  and st45 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 45)
      in
        case category of
          Integer _ => [] @ List.concat [st46r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 45) *)
      end
  and st44r0 ((String sv3, pos3, stNum3)::(Cat sv2, pos2, stNum2)::(MinimumSize sv1, pos1, stNum1)::(TerminatorKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Terminator ((pos0, pos), sv1, sv2, sv3))) (pos0, pos)
    | st44r0 stack pos = []
  and st43 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 43)
      in
        case category of
          String _ => [] @ List.concat [st44r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 43) *)
      end
  and st42 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 42)
      in
        case category of
          Cat _ => [(43, (stackItem::stack))]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 42) *)
      end
  and st41r0 ((String sv3, pos3, stNum3)::(Cat sv2, pos2, stNum2)::(MinimumSize sv1, pos1, stNum1)::(SeparatorKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Separator ((pos0, pos), sv1, sv2, sv3))) (pos0, pos)
    | st41r0 stack pos = []
  and st40 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 40)
      in
        case category of
          String _ => [] @ List.concat [st41r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 40) *)
      end
  and st39r0 ((NonemptyKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (MinimumSize (Ast.MNonempty ((pos0, pos)))) (pos0, pos)
    | st39r0 stack pos = []
  and st38 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 38)
      in
        case category of
          Cat _ => [(40, (stackItem::stack))]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 38) *)
      end
  and st37r0 ((String sv2, pos2, stNum2)::(String sv1, pos1, stNum1)::(CommentKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Comments ((pos0, pos), sv1, sv2))) (pos0, pos)
    | st37r0 stack pos = []
  and st36 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 36)
      in
        case category of
          String _ => [] @ List.concat [st37r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 36) *)
      end
  and st36r0 ((String sv1, pos1, stNum1)::(CommentKw, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Comment ((pos0, pos), sv1))) (pos0, pos)
    | st36r0 stack pos = []
  and st35r0 ((RParen, pos4, stNum4)::(RBracket, pos3, stNum3)::(LBracket, pos2, stNum2)::(Colon, pos1, stNum1)::(LParen, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListOne ((pos0, pos)))) (pos0, pos)
    | st35r0 stack pos = []
  and st34 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 34)
      in
        case category of
          RParen => [] @ List.concat [st35r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 34) *)
      end
  and st33 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 33)
      in
        case category of
          RBracket => [(34, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 33) *)
      end
  and st32r0 ((RParen, pos2, stNum2)::(Colon, pos1, stNum1)::(LParen, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListCons ((pos0, pos)))) (pos0, pos)
    | st32r0 stack pos = []
  and st31 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 31)
      in
        case category of
          RParen => [] @ List.concat [st32r0 (stackItem::stack) toPos]
        | LBracket => [(33, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 31) *)
      end
  and st30r0 ((RBracket, pos1, stNum1)::(LBracket, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.ListE ((pos0, pos)))) (pos0, pos)
    | st30r0 stack pos = []
  and st29r0 ((RBracket, pos2, stNum2)::(Cat sv1, pos1, stNum1)::(LBracket, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.ListCat ((pos0, pos), sv1))) (pos0, pos)
    | st29r0 stack pos = []
  and st28 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 28)
      in
        case category of
          RBracket => [] @ List.concat [st29r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 28) *)
      end
  and st27r0 ((Item' sv1, pos1, stNum1)::(Item sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item' (sv0::sv1)) (pos0, pos)
    | st27r0 stack pos = []
  and st26r0 ((Cat sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.NTerminal ((pos0, pos), sv0))) (pos0, pos)
    | st26r0 stack pos = []
  and st25r0 ((String sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Item (Ast.Terminal ((pos0, pos), sv0))) (pos0, pos)
    | st25r0 stack pos = []
  and st24 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 24)
      in
        case category of
          Item' _ => [] @ List.concat [st27r0 (stackItem::stack) toPos]
        | Item _ => [(24, (stackItem::stack))] @ List.concat [st24r0 (stackItem::stack) toPos]
        | String _ => [] @ List.concat [st25r0 (stackItem::stack) toPos]
        | Cat _ => [] @ List.concat [st26r0 (stackItem::stack) toPos]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 24) *)
      end
  and st24r0 stack pos = go 24 stack (Item' []) (pos, pos)
  and st23r0 ((Item' sv4, pos4, stNum4)::(As, pos3, stNum3)::(Cat sv2, pos2, stNum2)::(Dot, pos1, stNum1)::(Label sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def (Ast.Rule ((pos0, pos), sv0, sv2, sv4))) (pos0, pos)
    | st23r0 stack pos = []
  and st22 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 22)
      in
        case category of
          Item' _ => [] @ List.concat [st23r0 (stackItem::stack) toPos]
        | Item _ => [(24, (stackItem::stack))] @ List.concat [st24r0 (stackItem::stack) toPos]
        | String _ => [] @ List.concat [st25r0 (stackItem::stack) toPos]
        | Cat _ => [] @ List.concat [st26r0 (stackItem::stack) toPos]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 22) *)
      end
  and st22r0 stack pos = go 22 stack (Item' []) (pos, pos)
  and st21r0 ((Ident sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Cat (Ast.IdCat ((pos0, pos), sv0))) (pos0, pos)
    | st21r0 stack pos = []
  and st20 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 20)
      in
        case category of
          Cat _ => [(28, (stackItem::stack))]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 20) *)
      end
  and st19 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 19)
      in
        case category of
          As => [(22, (stackItem::stack))] @ List.concat [st22r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 19) *)
      end
  and st18 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 18)
      in
        case category of
          Cat _ => [(19, (stackItem::stack))]
        | LBracket => [(20, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st21r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 18) *)
      end
  and st17r0 ((Def' sv2, pos2, stNum2)::(Semi, pos1, stNum1)::(Def sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Def' (sv0::sv2)) (pos0, pos)
    | st17r0 stack pos = []
  and st16 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 16)
      in
        case category of
          Def' _ => [] @ List.concat [st17r0 (stackItem::stack) toPos]
        | Def _ => [(6, (stackItem::stack))]
        | Label _ => [(7, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st8r0 (stackItem::stack) toPos]
        | Underscore => [] @ List.concat [st9r0 (stackItem::stack) toPos]
        | LBracket => [(10, (stackItem::stack))]
        | LParen => [(11, (stackItem::stack))]
        | CommentKw => [(12, (stackItem::stack))]
        | SeparatorKw => [(13, (stackItem::stack))] @ List.concat [st13r0 (stackItem::stack) toPos]
        | TerminatorKw => [(14, (stackItem::stack))] @ List.concat [st14r0 (stackItem::stack) toPos]
        | CoercionsKw => [(15, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 16) *)
      end
  and st16r0 stack pos = go 16 stack (Def' []) (pos, pos)
  and st15 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 15)
      in
        case category of
          Ident _ => [(45, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 15) *)
      end
  and st14 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 14)
      in
        case category of
          MinimumSize _ => [(42, (stackItem::stack))]
        | NonemptyKw => [] @ List.concat [st39r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 14) *)
      end
  and st14r0 stack pos =
      go 14 stack (MinimumSize (Ast.MEmpty ((pos, pos)))) (pos, pos)
  and st13 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 13)
      in
        case category of
          MinimumSize _ => [(38, (stackItem::stack))]
        | NonemptyKw => [] @ List.concat [st39r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 13) *)
      end
  and st13r0 stack pos =
      go 13 stack (MinimumSize (Ast.MEmpty ((pos, pos)))) (pos, pos)
  and st12 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 12)
      in
        case category of
          String _ => [(36, (stackItem::stack))] @ List.concat [st36r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 12) *)
      end
  and st11 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 11)
      in
        case category of
          Colon => [(31, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 11) *)
      end
  and st10 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 10)
      in
        case category of
          RBracket => [] @ List.concat [st30r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 10) *)
      end
  and st9r0 ((Underscore, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Wild ((pos0, pos)))) (pos0, pos)
    | st9r0 stack pos = []
  and st8r0 ((Ident sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Label (Ast.Id ((pos0, pos), sv0))) (pos0, pos)
    | st8r0 stack pos = []
  and st7 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 7)
      in
        case category of
          Dot => [(18, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 7) *)
      end
  and st6 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 6)
      in
        case category of
          Semi => [(16, (stackItem::stack))] @ List.concat [st16r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 6) *)
      end
  and st5r0 ((Def' sv1, pos1, stNum1)::(Token' sv0, pos0, stNum0)::stack) pos =
      go stNum0 stack (Grammar (Ast.Grammar ((pos0, pos), sv0, sv1))) (pos0, pos)
    | st5r0 stack pos = []
  and st4 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 4)
      in
        case category of
          Ident _ => [(49, (stackItem::stack))] @ List.concat [st49r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 4) *)
      end
  and st3 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 3)
      in
        case category of
          Semi => [(47, (stackItem::stack))] @ List.concat [st47r0 (stackItem::stack) toPos]
        | c => [] (* raise Parse (c, pos, 3) *)
      end
  and st2 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 2)
      in
        case category of
          Def' _ => [] @ List.concat [st5r0 (stackItem::stack) toPos]
        | Def _ => [(6, (stackItem::stack))]
        | Label _ => [(7, (stackItem::stack))]
        | Ident _ => [] @ List.concat [st8r0 (stackItem::stack) toPos]
        | Underscore => [] @ List.concat [st9r0 (stackItem::stack) toPos]
        | LBracket => [(10, (stackItem::stack))]
        | LParen => [(11, (stackItem::stack))]
        | CommentKw => [(12, (stackItem::stack))]
        | SeparatorKw => [(13, (stackItem::stack))] @ List.concat [st13r0 (stackItem::stack) toPos]
        | TerminatorKw => [(14, (stackItem::stack))] @ List.concat [st14r0 (stackItem::stack) toPos]
        | CoercionsKw => [(15, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 2) *)
      end
  and st2r0 stack pos = go 2 stack (Def' []) (pos, pos)
  and st1r0 stack pos = [(~1, stack)]
  and st0 stack category (fromPos, toPos) =
      let
        val stackItem = (category, fromPos, 0)
      in
        case category of
          Grammar _ => [] @ List.concat [st1r0 (stackItem::stack) toPos]
        | Token' _ => [(2, (stackItem::stack))] @ List.concat [st2r0 (stackItem::stack) toPos]
        | Token _ => [(3, (stackItem::stack))]
        | TokenKw => [(4, (stackItem::stack))]
        | c => [] (* raise Parse (c, pos, 0) *)
      end
  and st0r0 stack pos = go 0 stack (Token' []) (pos, pos)
  fun parse sourcemap strm =
      let
        val pos = Lex.getPos strm
        val stacks = [(0, [])] @ List.concat [st0r0 [] pos]
        fun loop stacks strm =
            let
              val pos = Lex.getPos strm
              val (token, span, strm') = Lex.lex sourcemap strm
            in
              case token of
                Token.EOF =>
                let
                  val completeStacks = List.filter (fn (st, _) => st = ~1) stacks
                  val topCategories = map (fn (st, stack) => hd stack) completeStacks
                  fun toAst (Grammar sv, _, _) = SOME sv | toAst _ = NONE
                in
                  List.mapPartial toAst topCategories
                end
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
