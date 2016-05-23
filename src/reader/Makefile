MLTON=mlton

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc reader.mlb
	$(MLTON) -stop tc reader-sig.mlb
	$(MLTON) -stop tc datum-constructor-sig.mlb

.PHONY: clean
clean:
	make -C test clean
