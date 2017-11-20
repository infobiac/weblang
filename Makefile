hello-world: hello-world.o
	nix-shell -p gcc --command "gcc -lm hello-world.o -o hello-world"

hello-world.o: hello-world.s
	nix-shell -p gcc --command "gcc -c hello-world.s -o hello-world.o"

hello-world.s: hello-world.ll
	nix-shell -p llvm --command "llc hello-world.ll"

hello-world.ll: examples/crazy-hello-world.wl Build
	stack --nix exec weblang hello-world.ll < examples/crazy-hello-world.wl

Build: 
	stack --nix build


chapter3-bin: chapter3/chapter3.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ chapter3/chapter3.o jsonlib/jsonlib.o -o chapter3-bin"

chapter3/putd.o: jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ -c jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"

chapter3/chapter3.o: chapter3/chapter3.s
	nix-shell -p gcc --command "gcc -c chapter3/chapter3.s -o chapter3/chapter3.o"

chapter3/chapter3.s: chapter3/chapter3.ll
	nix-shell -p llvm --command "llc chapter3/chapter3.ll"

chapter3/chapter3.ll: Build chapter3/chapter3.k chapter3/Main.hs
	stack exec --nix chapter3 chapter3/chapter3.ll < chapter3/chapter3.k

.PHONY : clean
clean:
	rm *.o *.s *.ll hello-world 
