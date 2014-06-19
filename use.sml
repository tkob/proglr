(* this file can be used from REPL of Poly/ML, Moscow ML and SML/NJ *)

structure Position = Int; (* for Moscow ML *)

use "smlnj-lib/Util/utf8-sig.sml";
use "smlnj-lib/Util/utf8.sml";
use "ml-lpt/lib/stream-pos.sml";
use "ml-lpt/lib/antlr-lexer-sig.sml";
use "ml-lpt/lib/antlr-tokens-sig.sml";
use "ml-lpt/lib/ebnf.sml";
use "ml-lpt/lib/repair.sml";
use "ml-lpt/lib/ulex-buffer.sml";
use "ml-lpt/lib/wrapped-strm.sml";

use "cmlib/hash-inc.sig";
use "cmlib/hash-inc.sml";
use "cmlib/idict.sig";
use "cmlib/hashable.sig";
use "cmlib/hash-table.sig";
use "cmlib/hash-table.sml";

use "parse.sml";
use "scan.ulex.sml";
use "proglr.sml";
