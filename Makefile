.PHONY: all run pack clean out

CC=/opt/ghc/7.10.3/bin/ghc
# CC=ghc
OPTIONS = -O3 -outputdir out -ilib -o out/main -tmpdir .
SOURCES=$(shell find src app -type f -name "*.hs")

all: out 
	$(CC) $(OPTIONS) $(SOURCES)

run: 
	./out/main
out: 
	mkdir -p out

pack:
	zip hw0.zip -r Makefile $(SOURCES)

clean: 
	rm -rf out