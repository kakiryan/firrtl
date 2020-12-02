#!/bin/bash

sbt assembly
./utils/bin/firrtl -i Adder.hi.fir --custom-transforms traversals.TraverseAST
