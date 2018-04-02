.PHONY: all run pack clean out

HC=/opt/ghc/7.10.3/bin/ghc
# HC=ghc
OPTIONS =-O3 -outputdir out -ilib -o out/main -tmpdir . # -prof
SOURCES=$(shell find src app -type f -name "*.hs")

all: out 
	$(HC) $(OPTIONS) $(SOURCES)

run: 
	./out/main # +RTS -s

out: 
	mkdir -p out

pack:
	zip hw.zip -r Makefile $(SOURCES)

clean: 
	rm -rf out