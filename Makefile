mlglr: parse.sml scan.ulex.sml mlglr.sml boot.sml 
	mlton \
	-output 'mlglr' \
	-default-ann 'allowFFI true' \
	mlglr.mlb 
clean:
	rm -f mlglr
