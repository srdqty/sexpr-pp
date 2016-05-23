MLTON=mlton

.PHONY: sexpr-repl
sexpr-repl:
	mlton src/sexpr-repl.mlb
	mv src/sexpr-repl .

.PHONY: clean
clean:
	rm -f sexpr-repl
