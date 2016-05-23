MLTON=mlton

all: repl-test

.PHONY: repl-test
repl-test:
	$(MLTON) repl-test.mlb


.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc repl.mlb

.PHONY: clean
clean:
	rm -f repl-test
