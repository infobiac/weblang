import os
import filecmp
import datetime

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def compilefile(f,test,logfile):
    os.system('python testss/'+f + ' > test_output 2>&1')
#    os.system('make testss/'+f)
#    os.system('./'+test+' > test_output')
    os.system('echo "\n"[Testing '+ test+ ' at ' +str(datetime.datetime.now()) + '] >> ' +logfile)
    os.system('cat test_output>>'+ logfile)
    output = 'test_output'
    return output


##### START TEST SCRIPT #####

test_files = os.listdir("./testss")
expected_files = os.listdir("./expected")
testcount = 0
passed = 0
logfile = 'test_log'

for f in test_files:
    testcount+=1
    test = f.split('.')[0]
    output = compilefile(f,test,logfile)
    equal = filecmp.cmp(('expected/'+test),output)
    if(equal):
        print(bcolors.OKGREEN+"[Passed] "+test+bcolors.ENDC)
        passed+=1
    else:
        print(bcolors.FAIL+"[Failed] "+test+bcolors.ENDC)

os.system('rm -rf test_output')
print(bcolors.HEADER+bcolors.BOLD+"Passed "+str(passed)+" out of "+str(testcount)+" tests."+bcolors.ENDC)
