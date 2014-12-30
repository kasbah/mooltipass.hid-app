ELM_FILES = $(wildcard src/*.elm)
JS_FILES  = $(wildcard src/*.js)

all: dir images build/manifest.json elm js
	cp src/index.html build/

dir: build/.dirstamp

build/manifest.json: dir
	cp src/manifest.json build/

images: dir
	cp -r src/images build/

elm: build/elm.js
js: $(patsubst src/%, build/%, $(JS_FILES))

build/.dirstamp:
	mkdir -p build
	touch build/.dirstamp

build/elm.js:
	elm-make $(ELM_FILES) --output build/elm.js

build/%.js: src/%.js
	cp $< $@

clean:
	rm -rf build

.PHONY: all images dir js elm clean
