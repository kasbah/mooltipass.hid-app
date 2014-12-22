all: dir images build/manifest.json js
	elm-make src/main.elm --output build/elm.js
	cp src/index.html build/

js: $(patsubst src/%, build/%, $(wildcard src/*.js))

build/%.js: src/%.js
	cp $< $@

build/.dirstamp:
	mkdir -p build
	touch build/.dirstamp

build/manifest.json: dir
	cp src/manifest.json build/

images: dir
	mkdir -p build
	cp -r src/images build/

dir: build/.dirstamp

clean:
	rm -rf build

.PHONY: all images dir js clean
