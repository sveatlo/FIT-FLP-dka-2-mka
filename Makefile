.PHONY: all
all:
	ghc src/main.hs -o ./main

.PHONY: zip
zip: clean
	zip -r9 flp-fun-xhanze10.zip ./*

.PHONY: clean
clean:
	rm -rf src/main src/main.o src/main.hi ./main
