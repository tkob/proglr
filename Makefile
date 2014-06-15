mlglr: parse.sml scan.ulex.sml mlglr.sml boot.sml 
	mlton \
	-output 'mlglr' \
	-default-ann 'allowFFI true' \
	mlglr.mlb 
scan.ulex.sml: scan.ulex
	ml-ulex scan.ulex
clean:
	rm -f mlglr
