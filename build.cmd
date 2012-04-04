@echo off
mkdir bin
mkdir obj
mkdir hi
cd source
ghc -Wall -O3 -Werror -hidir ../hi/ -odir ../obj/ %1.hs -o ../bin/%1
haddock ./kryukovlib/haddock -w -h -v0 -o ../haddock/ --optghc=-no-hs-main 
cd ..
