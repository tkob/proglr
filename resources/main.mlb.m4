ifdef(`PROGLR_PARSE_SML', , `define(`PROGLR_PARSE_SML', `')')
ifdef(`PROGLR_SCAN_SML', , `define(`PROGLR_SCAN_SML', `')')
ifdef(`PROGLR_MAIN_SML', , `define(`PROGLR_MAIN_SML', `main.sml')')

$(SML_LIB)/basis/basis.mlb

ifelse(PROGLR_COMPILER, `mlton',
$(SML_LIB)/smlnj-lib/smlnj-lib.mlb
$(SML_LIB)/mllpt-lib/mllpt-lib.mlb
)

ifelse(PROGLR_COMPILER, `mlkit',
smlnj-lib/Util/utf8.mlb
ml-lpt/lib/stream-pos.sml
ml-lpt/lib/antlr-lexer-sig.sml
ml-lpt/lib/antlr-tokens-sig.sml
ml-lpt/lib/ebnf.sml
ml-lpt/lib/repair.sml
ml-lpt/lib/ulex-buffer.sml
ml-lpt/lib/wrapped-strm.sml
)

PROGLR_PARSE_SML
PROGLR_SCAN_SML
PROGLR_MAIN_SML
