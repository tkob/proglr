ifdef(`PROGLR_PARSE_SML', , `define(`PROGLR_PARSE_SML', `')')
ifdef(`PROGLR_SCAN_SML', , `define(`PROGLR_SCAN_SML', `')')
ifdef(`PROGLR_MAIN_SML', , `define(`PROGLR_MAIN_SML', `main.sml')')

$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/smlnj-lib/smlnj-lib.mlb
$(SML_LIB)/mllpt-lib/mllpt-lib.mlb

PROGLR_PARSE_SML
PROGLR_SCAN_SML
PROGLR_MAIN_SML
