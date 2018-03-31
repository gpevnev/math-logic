.PHONY: all run pack clean out

CC=/opt/ghc/7.10.3/bin/ghc
# CC=ghc
OPTIONS =-O228 -outputdir out -ilib -o out/main -tmpdir . # -prof
SOURCES=$(shell find src app -type f -name "*.hs")

all: out 
	$(CC) $(OPTIONS) $(SOURCES)

run: 
	./out/main # +RTS -s
out: 
	mkdir -p out

pack:
	zip hw.zip -r Makefile $(SOURCES)

clean: 
	rm -rf out