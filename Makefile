wildc_recursive=$(foreach d,$(wildcard $1*),$(call wildc_recursive,$d/,$2) $(filter $(subst *,%,$2),$d))

VERSION = 0.7.1
PACKAGE_NAME = mooltipass.hid-app.$(VERSION)

GUI_ELM_FILES    = $(wildcard src/gui/*.elm)
BG_ELM_FILES     = $(wildcard src/background/*.elm)
COMMON_ELM_FILES = $(wildcard src/common/*.elm)
JS_FILES         = $(call wildc_recursive, src/, *.js)
HTML_FILES       = $(call wildc_recursive, src/, *.html)
IMAGE_FILES      = $(wildcard src/gui/images/*)
DIRS             = $(sort $(dir $(call wildc_recursive, src/, *)))
PACKAGE_STAMPS   = $(patsubst src%, $(PACKAGE_NAME)%.dirstamp, $(DIRS))

all: images elm js manifest html

dirs   : elm-stuff/.core-linked $(patsubst src%, build%/.dirstamp, $(DIRS))
elm    : dirs build/background/elm-background.js build/gui/elm-gui.js
js     : dirs $(patsubst src/%, build/%, $(JS_FILES))
html   : dirs $(patsubst src/%, build/%, $(HTML_FILES))
images : dirs $(patsubst src/%, build/%, $(IMAGE_FILES))
manifest: build/manifest.json

build/manifest.json: src/manifest.json
	sed 's/@version/"$(VERSION)"/' src/manifest.json > build/manifest.json

%/.dirstamp:
	mkdir $*
	@touch $@

elm-stuff/.core-linked: elm-stuff/.dirstamp
	rm -rf elm-stuff/packages/elm-lang/core/1.1.0
	ln -s $(PWD)/elm-core $(PWD)/elm-stuff/packages/elm-lang/core/1.1.0
	@touch $@

elm-stuff/.dirstamp:
	elm-package install -y
	@touch $@

build/gui/elm-gui.js: $(GUI_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(GUI_ELM_FILES) --output $@

build/background/elm-background.js: $(BG_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(BG_ELM_FILES) --output $@

build/%: src/%
	cp $< $@

clean:
	rm -rf build
	rm -rf elm-stuff

package: all
	cp -r build $(PACKAGE_NAME)
	rm -f $(PACKAGE_STAMPS)
	rm -f $(PACKAGE_NAME)/gui/images/logo_square*.svg
	zip -r $(PACKAGE_NAME).zip $(PACKAGE_NAME)/
	rm -rf $(PACKAGE_NAME)/

.PHONY: all images dirs js manifest elm clean package
