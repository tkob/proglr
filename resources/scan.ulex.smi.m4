_require "basis.smi"
_require "smlnj-lib/Util/utf8.smi"
_require "ml-lpt/lib/stream-pos.smi"
_require "ml-lpt/lib/ulex-buffer.smi"
_require "PROGLR_PARSE_SMI"

structure Lexer = struct
  type strm (=boxed)
  type pos = int
  type span = pos * pos
  type tok = Token.token
  val lex : AntlrStreamPos.sourcemap -> strm -> tok * span * strm
  val getPos : strm -> pos
  val streamify : (unit -> string) -> strm
  val streamifyReader : (char, 'a) StringCvt.reader -> 'a -> strm
  val streamifyInstream : TextIO.instream -> strm
end
