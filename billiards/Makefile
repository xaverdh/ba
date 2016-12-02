all: demo fdemo totex

clean:
	@echo "cleaning up"
	rm ./hidir/*
	rm ./odir/*
	rm ./bin/*

demoopts := -idemo demo/demo.hs -odir odir -hidir hidir -o bin/demo
demo:
	cabal exec -- ghc -j4 -O2 $(demoopts)

texopts := -itotex totex/totex.hs -odir odir -hidir hidir -o bin/totex 
totex:
	cabal exec -- ghc -j4 -O2 $(texopts)

fdemoopts := -ifdemo fdemo/fdemo.hs -odir odir -hidir hidir -o bin/fdemo
fdemo:
	cabal exec -- ghc -j4 -O2 $(fdemoopts)

profileopts := -rtsopts -prof -fprof-auto
profile:
	cabal exec -- ghc -j4 -O2 $(profileopts) $(demoopts)
