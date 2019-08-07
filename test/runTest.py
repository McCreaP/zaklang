#!/usr/bin/python3
from subprocess import Popen, PIPE
import re
import os

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(TEST_DIR)
GOOD_DIR = os.path.join(TEST_DIR, 'good')

TEST_BASE_FILE = os.path.join(GOOD_DIR, 'test.zak')

ZAKLANG = os.path.join(BASE_DIR, "bin", "zaklang")


def main():
	with open(TEST_BASE_FILE, 'r') as f:
		testBase = f.read()
	scenarios = extractScenarios(testBase)
	for scenario in scenarios:
		runTest(testBase, scenario)


def extractScenarios(testBase):
	scenarios = []
	lineNumber = 0
	lines = testBase.splitlines()
	while lineNumber < len(lines):
		matchTestDescription = re.match(r'.*@Test: (.*)', lines[lineNumber])
		if matchTestDescription:
			lineNumber += 1
			matchFunctionName = re.match(r'def (.*?)\(\).*', lines[lineNumber])
			if matchFunctionName:
				scenarios.append({
					"description": matchTestDescription.group(1),
					"functionName": matchFunctionName.group(1) 
				})
		lineNumber += 1
	return scenarios


def runTest(testBase, scenario):
	print(bcolors.HEADER + "Test: " + bcolors.ENDC + scenario["description"] +
		(70 - len(scenario["description"])) * " ", end="")
	testData = prepareTestData(testBase, scenario["functionName"])
	test = Popen([ZAKLANG], cwd=BASE_DIR, stdout=PIPE, stdin=PIPE, stderr=PIPE)
	testResult = test.communicate(input=testData.encode())
	if testResult[0] == b"0\n":
		print(bcolors.OKGREEN + "SUCCESS" + bcolors.ENDC) 
	else:
	 	print(bcolors.FAIL + "FAIL" + bcolors.ENDC)
	 	if testResult[0]:
	 		print("stdout: " + testResult[0].decode())
	 	if testResult[1]:
	 		print("stderr: " + testResult[1].decode())


def prepareTestData(testBase, functionName):
	return testBase + "\ndef main() = if " + functionName + "() then 0 else 1"


class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


if __name__ == '__main__':
	main()
