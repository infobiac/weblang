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


json: json-example.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ json-example.o jsonlib/jsonlib.o -o json-example"

json-example.o: json-example.s
	nix-shell -p gcc --command "gcc -c json-example.s -o json-example.o"

json-example.s: json-example.ll
	nix-shell -p llvm --command "llc json-example.ll"

json-example.ll: examples/json-example.wl Build
	stack --nix exec weblang json-example.ll < examples/json-example.wl

jsonlib/jslonlib.o: jsonlib.cpp
	nix-shell -p gcc --command "g++ jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"



chapter3-bin: chapter3/chapter3.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ chapter3/chapter3.o jsonlib/jsonlib.o -o chapter3-bin"

chapter3/putd.o: jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ -c jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"

chapter3/chapter3.o: chapter3/chapter3.s
	nix-shell -p gcc --command "g++ -c chapter3/chapter3.s -o chapter3/chapter3.o"

chapter3/chapter3.s: chapter3/chapter3.ll
	nix-shell -p llvm --command "llc chapter3/chapter3.ll"

chapter3/chapter3.ll: Build chapter3/chapter3.k chapter3/Main.hs
	stack exec --nix chapter3 chapter3/chapter3.k chapter3/chapter3.ll




chapter3-test: chapter3/chapter3.o chapter3/test.o
	nix-shell -p gcc --command "g++ chapter3/chapter3.o chapter3/test.o -o chapter3-test"

chapter3/test.o: chapter3/test.cpp
	nix-shell -p gcc --command "g++ -c chapter3/test.cpp -o chapter3/test.o"

.PHONY : clean
clean:
	rm -f a.out *.o *.s *.ll hello-world jsonlib/*.o jsonlib/a.out chapter3/*.o chapter3/*.s chapter3/*.ll json-example 
