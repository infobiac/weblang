assignment: assignment.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ assignment.o jsonlib/jsonlib.o -o assignment"

hello-world: hello-world.o
	nix-shell -p gcc --command "gcc -lm hello-world.o -o hello-world"

json: json-example.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ json-example.o jsonlib/jsonlib.o -o json-example"

newtype: newtype.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ newtype.o jsonlib/jsonlib.o -o newtype"

array: array.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ array.o jsonlib/jsonlib.o -o array"

conditional: conditional-example.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ jsonlib/jsonlib.o conditional-example.o -o conditional-example"

conditional-example.o: conditional-example.s
	nix-shell -p gcc --command "gcc -c conditional-example.s -o conditional-example.o" 

conditional-example.s: conditional-example.ll
	nix-shell -p llvm --command "llc conditional-example.ll"

conditional-example.ll: examples/conditional-example.wl Build-weblang
	stack --nix exec weblang conditional-example.ll < examples/conditional-example.wl


binop: binop-example.o jsonlib/jsonlib.o
	nix-shell -p gcc --command "g++ binop-example.o jsonlib/jsonlib.o -o binop-example"

binop-example.o: binop-example.s
	nix-shell -p gcc --command "gcc -c binop-example.s -o binop-example.o" 

binop-example.s: binop-example.ll
	nix-shell -p llvm --command "llc binop-example.ll"

binop-example.ll: examples/binop-example.wl Build-weblang
	stack --nix exec weblang binop-example.ll < examples/binop-example.wl


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

forloop: forloop.o jsonlib/jsonlib.o 
	nix-shell -p curl gcc --command "g++ forloop.o jsonlib/jsonlib.o -o forloop"

echo-server: echo.o client/client.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ echo.o client/client.o jsonlib/jsonlib.o -o echo-server -L/home/jordanvega/plt/client/cpr-example/build/lib -lcpr -lcurl" 

echo.o: echo.s
	nix-shell -p gcc --command "gcc -c echo.s -o echo.o"

echo.s: echo.ll
	nix-shell -p llvm --command "llc echo.ll"

echo.ll: examples/echo-example.wl Build-weblang
	stack --nix exec weblang echo.ll < examples/echo-example.wl

client/client.o: client/client.cpp
	nix-shell -p rapidjson gcc --command "gcc -Wall $$NIX_CFLAGS_COMPILE -c client/client.cpp -I/home/jordanvega/plt/client/cpr-example/opt/cpr/include -Iclient/cpr-example/opt/json/src -Lclient/cpr-example/build/lib -lcpr -lcurl  -o client/client.o"

functions: functions.o jsonlib/jsonlib.o
	nix-shell -p curl gcc --command "g++ functions.o jsonlib/jsonlib.o -o functions" 

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

server:
	./makeServer.sh && echo "running server at 35.194.4.65:8000/ \n" &&./runWeblangServer 8000

slack: slack.o client/client.o jsonlib/jsonlib.o
		nix-shell -p curl gcc --command "g++ slack.o client/client.o jsonlib/jsonlib.o -o slack -L/home/jordanvega/plt/client/cpr-example/build/lib -lcpr -lcurl"

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

