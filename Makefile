gen.sml: mlglr
	./mlglr gen.sml        
mlglr: mlglr.sml boot.sml 
	mlton \
	-output 'mlglr' \
	-default-ann 'allowFFI true' \
	mlglr.mlb 
clean:
	rm -f mlglr
