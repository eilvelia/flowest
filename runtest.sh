#!/bin/sh

EXIT_STATUS=0
cd ./_build/default/test
./TestRunner.exe "$@" || EXIT_STATUS=$?
cd ../../..
exit $EXIT_STATUS
