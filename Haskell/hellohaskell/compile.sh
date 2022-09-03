#!/bin/bash

ProgName="hellohaskell"

#Interpret 
#runghc $Progname".hs"

#Compile and Execute
ghc $ProgName".hs" -o $ProgName".run"
./$ProgName".run"
