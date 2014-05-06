gen.sml: mlbnfc
	./mlbnfc gen.sml        
mlbnfc: mlbnfc.sml boot.sml 
	mlton \
	-output 'mlbnfc' \
	-default-ann 'allowFFI true' \
	mlbnfc.mlb 
clean:
	rm -f mlbnfc
