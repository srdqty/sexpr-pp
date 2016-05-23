MLTON=mlton

.PHONY: typecheck
typecheck:
	make -C ast typecheck
	make -C compile-env typecheck
	make -C compiler typecheck
	make -C test typecheck

.PHONY: clean
clean:
	make -C ast clean
	make -C compile-env clean
	make -C compiler clean
	make -C test clean
