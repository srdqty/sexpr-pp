MLTON=mlton

.PHONY: typecheck
typecheck:
	make -C eval-env typecheck
	make -C eval typecheck
	make -C test typecheck

.PHONY: clean
clean:
	make -C eval-env clean
	make -C eval clean
	make -C test clean
