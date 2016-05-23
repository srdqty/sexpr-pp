MLTON=mlton

all: compiler-test

.PHONY: compiler-test
compiler-test:
	$(MLTON) compiler-test.mlb

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc compiler-test.mlb

.PHONY: clean
clean:
	rm -f compiler-test
