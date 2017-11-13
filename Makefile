hello-world: hello-world.o
	nix-shell -p gcc --command "gcc -lm hello-world.o -o hello-world"

hello-world.o: hello-world.s
	nix-shell -p gcc --command "gcc -c hello-world.s -o hello-world.o"

hello-world.s: hello-world.ll
	nix-shell -p llvm --command "llc hello-world.ll"

hello-world.ll: examples/crazy-hello-world.wl .stack-work/install/x86_64-linux-nix/lts-9.12/8.0.2/bin/weblang
	stack build && stack exec weblang hello-world.ll < examples/crazy-hello-world.wl


chapter3-bin: chapter3/chapter3.o
	nix-shell -p gcc --command "gcc -lm chapter3/chapter3.o chapter3/putd.o -o chapter3-bin"

chapter3/putd.o: chapter3/putd.c
	nix-shell -p gcc --command "gcc -c chapter3/putd.c -o chapter3/putd.o"

chapter3/chapter3.o: chapter3/chapter3.s
	nix-shell -p gcc --command "gcc -c chapter3/chapter3.s -o chapter3/chapter3.o"

chapter3/chapter3.s: chapter3/chapter3.ll
	nix-shell -p llvm --command "llc chapter3/chapter3.ll"

chapter3/chapter3.ll: chapter3/chapter3.k chapter3/Main.hs
	stack build && stack exec chapter3 chapter3/chapter3.k chapter3/chapter3.ll
