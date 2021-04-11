.PHONY: all
all:
	ghc src/main.hs -o ./dka-2-mka

.PHONY: zip
zip: clean
	zip -r9 flp-fun-xhanze10.zip ./*

.PHONY: clean
clean:
	rm -rf src/main src/*.hi src/*.o ./dka-2-mka
