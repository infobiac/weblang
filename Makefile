hello-world: hello-world.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ -lm hello-world.o jsonlib/jsonlib.o -o hello-world"

json: json-example.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ json-example.o jsonlib/jsonlib.o -o json-example"

newtype: newtype.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ newtype.o jsonlib/jsonlib.o -o newtype"

conditional: conditional-example.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ jsonlib/jsonlib.o conditional-example.o -o conditional-example"

binop: binop-example.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ binop-example.o jsonlib/jsonlib.o -o binop-example"

array: array.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ array.o jsonlib/jsonlib.o -o array"

forloop: forloop.o jsonlib/jsonlib.o 
	nix-shell -p curl gcc --command "g++ forloop.o jsonlib/jsonlib.o -o forloop"

echo-server: echo.o client/client.o
	nix-shell -p curl gcc --command "g++ echo.o client/client.o -o echo-server -L/home/jordanvega/plt/client/cpr-example/build/lib -lcpr -lcurl" 

echo.o: echo.s
	nix-shell -p gcc --command "gcc -c echo.s -o echo.o"

echo.s: echo.ll
	nix-shell -p llvm --command "llc echo.ll"

echo.ll: examples/echo-example.wl Build-weblang
	stack --nix exec weblang echo.ll < examples/echo-example.wl

client/client.o: client/client.cpp
	nix-shell -p rapidjson gcc --command "gcc -Wall $$NIX_CFLAGS_COMPILE -c client/client.cpp -I/home/jordanvega/plt/client/cpr-example/opt/cpr/include -Iclient/cpr-example/opt/json/src -Lclient/cpr-example/build/lib -lcpr -lcurl  -o client/client.o"

functions: functions.o
	nix-shell -p curl gcc --command "gcc functions.o -o functions" 

functions.o: functions.s
	nix-shell -p gcc --command "gcc -c functions.s -o functions.o"

functions.s: functions.ll
	nix-shell -p llvm --command "llc functions.ll"

functions.ll: examples/functions-example.wl Build-weblang
	stack --nix exec weblang functions.ll < examples/functions-example.wl

functions-crazy: functions-crazy.o
	nix-shell -p curl gcc --command "gcc functions-crazy.o -o functions-crazy" 

functions-crazy.o: functions-crazy.s
	nix-shell -p gcc --command "gcc -c functions-crazy.s -o functions-crazy.o"

functions-crazy.s: functions-crazy.ll
	nix-shell -p llvm --command "llc functions-crazy.ll"

functions-crazy.ll: examples/functions-crazy-example.wl Build-weblang
	stack --nix exec weblang functions-crazy.ll < examples/functions-crazy-example.wl

Build-weblang: 
	stack build --nix :weblang

jsonlib/jsonlib.o: jsonlib/jsonlib.cpp
	nix-shell -p rapidjson gcc --command "g++ $$NIX_CFLAGS_COMPILE -c jsonlib/jsonlib.cpp -o jsonlib/jsonlib.o"

%.o : %.s
	nix-shell -p gcc --command "g++ -c $< -o $@"

%.s: %.ll
	nix-shell -p llvm --command "llc $<"

%.ll: examples/%.wl Build-weblang
	stack --nix exec weblang $@ < $<

.PRECIOUS: %.ll

.PHONY : clean
clean:
	rm -f a.out chapter3-test *.o *.s *.ll hello-world jsonlib/*.o jsonlib/a.out chapter3/*.o chapter3/*.s chapter3/*.ll json-example chapter3-bin client/client.o functions functions-crazy newtype conditional-example array binop-example
