#!/bin/bash

rm -f CMakeCache.txt
cmake . -DPython3_EXECUTABLE=`which python3` -DSunway=0
