MLTON=mlton

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc lexer.mlb
	$(MLTON) -stop tc lexer-sig.mlb

.PHONY: clean
clean:
	make -C test clean
