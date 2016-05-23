MLTON=mlton

all: symbol-table-test

.PHONY: symbol-table-test
symbol-table-test:
	$(MLTON) symbol-table-test.mlb

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc symbol-table-test.mlb

.PHONY: clean
clean:
	rm -f symbol-table-test
