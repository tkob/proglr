Group is
  $/basis.cm
  $/smlnj-lib.cm
  $/ml-lpt-lib.cm
  ifdef(`PROGLR_PARSE_SML', PROGLR_PARSE_SML,)
  ifdef(`PROGLR_SCAN_ULEX', PROGLR_SCAN_ULEX : ml-ulex,)
  ifdef(`PROGLR_MAIN_SML', PROGLR_MAIN_SML, main.sml)
