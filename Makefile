all: dir images build/manifest.json build/main.js
	elm-make src/main.elm --output build/elm.js
	install src/index.html build/

js: $(patsubst src/%, build/%, $(wildcard src/*.js))

build/%.js: src/%.js
	install $< $@

build/.dirstamp:
	mkdir -p build
	touch build/.dirstamp

build/manifest.json: dir
	install src/manifest.json build/

images: dir
	mkdir -p build
	cp -r src/images build/

dir: build/.dirstamp

.PHONY: all images dir js
