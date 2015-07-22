FILES = \
smlnj-lib/Util/utf8-sig.sml \
smlnj-lib/Util/utf8.sml \
ml-lpt/lib/stream-pos.sml \
ml-lpt/lib/antlr-lexer-sig.sml \
ml-lpt/lib/antlr-tokens-sig.sml \
ml-lpt/lib/ebnf.sml \
ml-lpt/lib/repair.sml \
ml-lpt/lib/ulex-buffer.sml \
ml-lpt/lib/wrapped-strm.sml \
PROGLR_PARSE_SML \
PROGLR_SCAN_SML \
main.sml

main: PROGLR_PARSE_SML PROGLR_SCAN_SML main.sml 
	expect -f polybuild.tcl $(FILES)
	polyc -o main a.o

debug: PROGLR_PARSE_SML PROGLR_SCAN_SML main.sml 
	env DEBUG=1 expect -f polybuild.tcl $(FILES)

PROGLR_SCAN_SML: PROGLR_SCAN_ULEX
	ml-ulex PROGLR_SCAN_ULEX

clean:
	rm -f main
	rm -f *.o
