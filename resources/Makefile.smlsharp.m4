# Makefile for separate compilation with SML#.

SMLSHARP = smlsharp
SMLSHARPFLAGS = 
DEPEND = Makefile.smlsharp.depend

EXEC = main

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

OBJS = $(SRCS:.sml=.o)

all: $(EXEC)

$(EXEC): $(EXEC).smi $(OBJS)
	$(SMLSHARP) $(SMLSHARPFLAGS) -o $@ $(EXEC).smi

scan.ulex.sml: scan.ulex
	ml-ulex scan.ulex

%.o: %.sml
	$(SMLSHARP) $(SMLSHARPFLAGS) -c $<

depend:
	$(SMLSHARP) -MM $(SRCS) > $(DEPEND)

include $(DEPEND)

clean:
	rm -f $(OBJS) $(EXEC)
