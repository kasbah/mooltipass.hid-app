wildc_recursive=$(foreach d,$(wildcard $1*),$(call wildc_recursive,$d/,$2) $(filter $(subst *,%,$2),$d))

GUI_ELM_FILES    = $(wildcard src/gui/*.elm)
BG_ELM_FILES     = $(wildcard src/background/*.elm)
COMMON_ELM_FILES = $(wildcard src/common/*.elm)
JS_FILES         = $(call wildc_recursive, src/, *.js)
HTML_FILES       = $(call wildc_recursive, src/, *.html)
JSON_FILES       = $(call wildc_recursive, src/, *.json)
IMAGE_FILES      = $(call wildc_recursive, src/gui/images/, *)

all: dir images build/manifest.json elm js json html

dir: build/.dirstamp build/gui/.dirstamp build/background/.dirstamp build/gui/images/.dirstamp

elm: build/background/elm.js build/gui/elm.js
js: $(patsubst src/%, build/%, $(JS_FILES))
html: $(patsubst src/%, build/%, $(HTML_FILES))
json: $(patsubst src/%, build/%, $(JSON_FILES))
images: $(patsubst src/%, build/%, $(IMAGE_FILES))

build/.dirstamp:
	mkdir -p build/
	touch build/.dirstamp

build/gui/.dirstamp:
	mkdir -p build/gui
	touch build/gui/.dirstamp

build/gui/images/.dirstamp:
	mkdir -p build/gui/images
	touch build/gui/images/.dirstamp

build/background/.dirstamp:
	mkdir -p build/background
	touch build/background/.dirstamp

build/gui/elm.js: $(GUI_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(GUI_ELM_FILES) --output build/gui/elm.js

build/background/elm.js: $(BG_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(BG_ELM_FILES) --output build/background/elm.js

build/%.js: src/%.js
	cp $< $@

build/%.html: src/%.html
	cp $< $@

build/%.json: src/%.json
	cp $< $@

build/%.json: src/%.json
	cp $< $@

build/gui/images/%: src/gui/images/%
	cp $< $@

clean:
	rm -rf build

.PHONY: all images dir js elm clean
