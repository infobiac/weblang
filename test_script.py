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

def compilefile(f,test,logfile,test_files,detailed_logs):
    os.system('echo "\n"[Testing '+ test+ ' at ' +str(datetime.datetime.now()) + '] >> ' +logfile)
    os.system('./weblang '+test_files+'/'+test+'.wl > errors_warnings 2>&1' ) 
    os.system('./'+test+' test'+test+' a'+' > test_output 2>&1')
    #os.system('cat errors_warnings')
    #os.system('cat test_output')
    os.system('cat test_output>>'+ logfile)
    os.system('cat errors_warnings>>'+ detailed_logs)
    output = 'test_output'
    return output


##### START TEST SCRIPT #####
test_files = "test/tests"
expected_files = "test/expected"
tests = os.listdir(test_files)
expected = os.listdir(expected_files)
testcount = 0
passed = 0
logfile = 'test/test_log'
detailed_logs = 'test/detailed_log'
os.system('echo "STARTING TEST" > '+logfile)
for f in tests:
    if('.wl' in f):
        testcount+=1
        test = f.split('.')[0]
        output = compilefile(f,test,logfile,test_files,detailed_logs)
        equal = filecmp.cmp((expected_files+'/'+test),output)
        if(equal):
            print(bcolors.OKGREEN+"[Passed] "+test+bcolors.ENDC)
            passed+=1
            os.system('rm '+test)
        else:
            print(bcolors.FAIL+"[Failed] "+test+bcolors.ENDC)

os.system('rm  test_output')
os.system('rm  errors_warnings')
os.system('echo "----------------------- NEW TEST ------------------------------">>'+ logfile)
print(bcolors.HEADER+bcolors.BOLD+"Passed "+str(passed)+" out of "+str(testcount)+" tests."+bcolors.ENDC)
try:
    assert(passed == testcount)
except AssertionError:
    exit(1)
