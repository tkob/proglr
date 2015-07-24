# Unix Makefile stub for separate compilation with Moscow ML.  

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/share/mosml/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

SRCS= \
position.sml \
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

# this variable should list each unit in the program, 
# in the correct order, with an indication of the mode (-structure or -toplevel) 
# of each unit

UNITS= -toplevel $(SRCS:.sml=)

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo
 
all: main

main: main.uo
	$(MOSMLL) -o main $(SRCS:.sml=.uo)

clean:
	rm -f $(SRCS:.sml=.ui)
	rm -f $(SRCS:.sml=.uo)
	rm -f Makefile.mosml.bak

# these rules are only needed if UNITS is undefined or empty
.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.mosml.bak
	mv Makefile.mosml Makefile.mosml.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.mosml.bak > Makefile.mosml
	$(MOSMLTOOLS)/mosmldep $(UNITS) >> Makefile.mosml

### DO NOT DELETE THIS LINE
