@echo off
mkdir bin
mkdir obj
mkdir hi
ghc -Wall -O3 -Werror -isource -hidir hi -odir obj source/runtest.hs -o bin/runtest
cd source
haddock runtest -w -h -o ../haddock/
cd ..