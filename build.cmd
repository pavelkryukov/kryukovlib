@echo off
mkdir bin
mkdir obj
mkdir hi
cd source
ghc -Wall -O3 -Werror  -hidir ../hi/ -odir ../obj/ runtest.hs -o ../bin/runtest
haddock runtest -w -h -o ../haddock/
cd ..