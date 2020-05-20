package = hsdatalog
match =

configure:
	cabal configure

build: configure
	cabal build

test: configure
	cabal test

clean:
	cabal clean

ghci: configure
	cabal repl lib:hsdatalog

ghcid: configure
ifeq ($(match),)
	ghcid -c "cabal repl" --allow-eval --warnings
else
	ghcid -c "cabal repl" --allow-eval --warnings --test $(match)
endif

ghcid-test-suite: build
	ghcid -c "cabal repl lib:hsdatalog test:test" --allow-eval --warnings

.PHONY : configure build test clean ghci ghcid
