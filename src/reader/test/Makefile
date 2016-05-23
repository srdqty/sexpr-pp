MLTON=mlton

all: read_test

.PHONY: read_test
read_test:
	$(MLTON) read_test.mlb

.PHONY: typecheck
typecheck:
	$(MLTON) -stop tc read_test.mlb

.PHONY: clean
clean:
	rm -f read_test
