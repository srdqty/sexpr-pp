MLTON=mlton

all: expand-compile-eval-test

.PHONY: expand-compile-eval-test
expand-compile-eval-test:
	$(MLTON) expand-compile-eval-test.mlb

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc expand-compile-eval-test.mlb

.PHONY: clean
clean:
	rm -f expand-compile-eval-test
