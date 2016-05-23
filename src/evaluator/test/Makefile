MLTON=mlton

all: eval-test

.PHONY: eval-test
eval-test:
	$(MLTON) eval-test.mlb

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc eval-test.mlb

.PHONY: clean
clean:
	rm -f eval-test
