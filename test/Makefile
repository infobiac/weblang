#testArg: testArg.o ../jsonlib/jsonlib.o
#	nix-shell -p gcc --command "g++ testArg.o ../jsonlib/jsonlib.o -o testArg"

#testLog: testLog.o ../jsonlib/jsonlib.o
#	nix-shell -p curl gcc --command "g++ testLog.o ../jsonlib/jsonlib.o -o testLog" 

#testVar: testVar.o ../jsonlib/jsonlib.o
#	nix-shell -p curl gcc --command "g++ testVar.o ../jsonlib/jsonlib.o -o testVar" 

#testFor: testFor.o ../jsonlib/jsonlib.o
#	nix-shell -p curl gcc --command "g++ testFor.o ../jsonlib/jsonlib.o -o testFor" 

%.o : %.s
		nix-shell -p gcc --command "g++ -c $< -o $@"

%.s: %.ll
		nix-shell -p llvm --command "llc $<"

%.ll: examples/%.wl Build-weblang
	stack --nix exec weblang $@ < $<
.PRECIOUS: %.ll

.PHONY : clean
clean:
	rm -f a.out chapter3-test *.o *.s *.ll hello-world jsonlib/*.o jsonlib/a.out chapter3/*.o chapter3/*.s chapter3/*.ll json-example chapter3-bin client/client.o functions functions-crazy newtype conditional-example array binop-example testLog

