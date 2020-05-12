package = hsdatalog
match =

configure:
	cabal configure

build:
	cabal build

test:
	cabal test

clean:
	cabal clean

ghci:
	cabal repl lib:hsdatalog

ghcid: configure
ifeq ($(match),)
	ghcid -c "cabal repl" --allow-eval --warnings
else
	ghcid -c "cabal repl" --allow-eval --warnings --test $(match)
endif

.PHONY : configure build test clean ghci ghcid
