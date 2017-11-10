chapter3-bin: chapter3/chapter3.o
	nix-shell -p gcc --command "gcc chapter3/chapter3.o -o chapter3-bin"

chapter3/chapter3.o: chapter3/chapter3.s
	nix-shell -p gcc --command "gcc -c chapter3/chapter3.s -o chapter3/chapter3.o"

chapter3/chapter3.s: chapter3/chapter3.ll
	nix-shell -p llvm --command "llc chapter3/chapter3.ll"

chapter3/chapter3.ll: chapter3/chapter3.k Built
	stack exec chapter3 chapter3/chapter3.k chapter3/chapter3.ll

Built:
	stack build
