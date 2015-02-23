
.PHONY: default

default: cabal.sandbox.config build
	cabal install -j1
	# cabal install -j1 --enable-documentation

cabal.sandbox.config build:
	cabal sandbox init --sandbox=build
