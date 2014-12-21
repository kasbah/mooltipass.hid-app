all: build/.dirstamp images
	elm-make src/main.elm --output build/elm.js
	install src/index.html build/

build/.dirstamp:
	mkdir -p build
	touch build/.dirstamp

images: build/.dirstamp
	mkdir -p build
	cp -r src/images build/

.PHONY: all images
