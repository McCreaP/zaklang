#!/usr/bin/python3
from subprocess import Popen, PIPE
import re
import os

from runTest import bcolors

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
BASE_DIR = os.path.dirname(TEST_DIR)
GOOD_DIR = os.path.join(TEST_DIR, 'good')
BAD_DIR = os.path.join(TEST_DIR, 'bad')

GOOD_BASE_FILE = os.path.join(GOOD_DIR, 'types.zak')
BAD_BASE_FILE = os.path.join(BAD_DIR, 'types.zak')

PRINT_TYPES = os.path.join(BASE_DIR, "bin", "printTypes")


def main():
	runTestsFrom(GOOD_BASE_FILE)
	runTestsFrom(BAD_BASE_FILE)


def runTestsFrom(testFile):
	with open(testFile, 'r') as f:
		typesBase = f.read()
	scenarios = extractScenarios(typesBase)
	for scenario in scenarios:
		printTypes(scenario)

def extractScenarios(typesBase):
	scenarios = []
	lines = typesBase.splitlines()
	startSection, description = firstSection(lines, 0)
	while startSection < len(lines):
		newStartSection, newDescription = firstSection(lines, startSection + 1)
		scenarios.append({
			"description": description,
			"body": '\n'.join(lines[startSection : newStartSection]) 
		})
		startSection = newStartSection
		description = newDescription
	return scenarios



def firstSection(lines, lineNumber):
	while lineNumber < len(lines) and not matchHeader(lines[lineNumber]):
		lineNumber += 1
	description = matchHeader(lines[lineNumber]).group(1) if lineNumber < len(lines) else None
	return lineNumber, description


def matchHeader(line):
	return re.match(r'.*@Types: (.*)', line)


def printTypes(scenario):
	print(bcolors.HEADER + "Types: " + scenario["description"] + bcolors.ENDC)
	# print(scenario['body'])
	types = Popen([PRINT_TYPES], cwd=BASE_DIR, stdout=PIPE, stdin=PIPE, stderr=PIPE)
	typesResult = types.communicate(input=scenario['body'].encode())
	if typesResult[0]:
		print(typesResult[0].decode())
	if typesResult[1]:
		print(bcolors.FAIL + typesResult[1].decode() + bcolors.ENDC)

if __name__ == '__main__':
	main()
