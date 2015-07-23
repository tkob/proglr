smlnj-lib/Util/utf8.alc: smlnj-lib/Util/utf8-sig.alc

ml-lpt/lib/antlr-lexer-sig.alc: ml-lpt/lib/stream-pos.alc
ml-lpt/lib/ebnf.alc: ml-lpt/lib/stream-pos.alc
ml-lpt/lib/repair.alc: ml-lpt/lib/stream-pos.alc
ml-lpt/lib/ulex-buffer.alc: ml-lpt/lib/stream-pos.alc
ml-lpt/lib/wrapped-strm.alc: ml-lpt/lib/antlr-tokens-sig.alc ml-lpt/lib/antlr-lexer-sig.alc ml-lpt/lib/repair.alc ml-lpt/lib/stream-pos.alc

PROGLR_PARSE_ALC: ml-lpt/lib/stream-pos.alc
PROGLR_SCAN_ALC: PROGLR_PARSE_ALC ml-lpt/lib/ulex-buffer.alc smlnj-lib/Util/utf8.alc ml-lpt/lib/stream-pos.alc

main.alc: PROGLR_PARSE_ALC PROGLR_SCAN_ALC ml-lpt/lib/stream-pos.alc
