
.PHONY: default

default: cabal.sandbox.config build
	cabal install --enable-documentation

cabal.sandbox.config build:
	cabal sandbox init --sandbox=build
