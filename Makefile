wildc_recursive=$(foreach d,$(wildcard $1*),$(call wildc_recursive,$d/,$2) $(filter $(subst *,%,$2),$d))

GUI_ELM_FILES    = $(wildcard src/gui/*.elm)
BG_ELM_FILES     = $(wildcard src/background/*.elm)
COMMON_ELM_FILES = $(wildcard src/common/*.elm)
JS_FILES         = $(call wildc_recursive, src/, *.js)

all: dir images build/manifest.json elm js
	cp src/gui/index.html build/gui/index.html

dir: build/.dirstamp build/gui/.dirstamp build/background/.dirstamp

build/manifest.json: dir
	cp src/manifest.json build/

images: dir
	cp -r src/gui/images build/gui/

elm: build/background/elm.js build/gui/elm.js
js: $(patsubst src/%, build/%, $(JS_FILES))

build/.dirstamp:
	mkdir -p build/
	touch build/.dirstamp

build/gui/.dirstamp:
	mkdir -p build/gui
	touch build/gui/.dirstamp

build/background/.dirstamp:
	mkdir -p build/background
	touch build/background/.dirstamp

build/gui/elm.js: $(GUI_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(GUI_ELM_FILES) --output build/gui/elm.js

build/background/elm.js: $(BG_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(BG_ELM_FILES) --output build/background/elm.js

build/%.js: src/%.js
	cp $< $@

clean:
	rm -rf build

.PHONY: all images dir js elm clean
