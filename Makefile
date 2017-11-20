hello-world: hello-world.o
	nix-shell -p gcc --command "gcc -lm hello-world.o -o hello-world"

hello-world.o: hello-world.s
	nix-shell -p gcc --command "gcc -c hello-world.s -o hello-world.o"

hello-world.s: hello-world.ll
	nix-shell -p llvm --command "llc hello-world.ll"

hello-world.ll: examples/hello-world.wl Build-weblang
	stack exec weblang hello-world.ll < examples/hello-world.wl

Build-weblang: 
	stack build :weblang


json: json-example.o jsonlib/jsonlib.o client/client.o
	nix-shell -p curl gcc --command "g++ json-example.o jsonlib/jsonlib.o client/client.o -o json-example -L./client -lrequests -L/usr/bin -lcurl"

json-example.o: json-example.s
	nix-shell -p gcc --command "gcc -c json-example.s -o json-example.o"

json-example.s: json-example.ll
	nix-shell -p llvm --command "llc json-example.ll"

json-example.ll: examples/json-example.wl Build-weblang
	stack --nix exec weblang json-example.ll < examples/json-example.wl

jsonlib/jsonlib.o: jsonlib/jsonlib.cpp
	nix-shell -p rapidjson gcc --command "g++ $$NIX_CFLAGS_COMPILE -c jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"

chapter3-bin: chapter3/chapter3.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ chapter3/chapter3.o jsonlib/jsonlib.o -o chapter3-bin"

chapter3/putd.o: jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ -c jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"

chapter3/chapter3.o: chapter3/chapter3.s
	nix-shell -p gcc --command "g++ -c chapter3/chapter3.s -o chapter3/chapter3.o"

chapter3/chapter3.s: chapter3/chapter3.ll
	nix-shell -p llvm --command "llc chapter3/chapter3.ll"

chapter3/chapter3.ll: Build-chapter3 chapter3/chapter3.k chapter3/Main.hs
	stack exec --nix chapter3 chapter3/chapter3.k chapter3/chapter3.ll

Build-chapter3:
	stack build :chapter3



chapter3-test: chapter3/chapter3.o chapter3/test.o
	nix-shell -p gcc --command "g++ chapter3/chapter3.o chapter3/test.o -o chapter3-test"

chapter3/test.o: chapter3/test.cpp
	nix-shell -p gcc --command "g++ -c chapter3/test.cpp -o chapter3/test.o"

client/client.o: client/client.c
	gcc -Wall -c client/client.c -L./client -lrequests -L/usr/bin -lcurl -o client/client.o	

.PHONY : clean
clean:
	rm -f a.out chapter3-test *.o *.s *.ll hello-world jsonlib/*.o jsonlib/a.out chapter3/*.o chapter3/*.s chapter3/*.ll json-example chapter3-bin client/client.o

