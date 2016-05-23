MLTON=mlton

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc interpreter-state-types-sig.mlb
	$(MLTON) -stop tc interpreter-state-types.mlb
	$(MLTON) -stop tc interpreter-state-sig.mlb
	$(MLTON) -stop tc interpreter-state.mlb

.PHONY: clean
clean:
