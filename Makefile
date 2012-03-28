BIN=jw
DIST=dist/build/$(BIN)/$(BIN)

all : $(BIN)

install : $(DIST)
	cabal install

$(BIN) : $(DIST)
	cp $< $@

dist/setup-config : jsonwrench.cabal
	cabal configure

$(DIST) : dist/setup-config jsonwrench.hs
	cabal build
	@touch $@ # cabal doesn't always update the build (if it doesn't need to)
