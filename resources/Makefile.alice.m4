SRCS = \
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

OBJS = $(SRCS:.sml=.alc)

all: $(OBJS)

%.alc:	%.sml 
	alicec --dependency-file main.depend --no-warn-conventions $< -o $@ 

clean:
	rm -f $(OBJS)

