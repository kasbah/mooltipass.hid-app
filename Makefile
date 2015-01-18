wildc_recursive=$(foreach d,$(wildcard $1*),$(call wildc_recursive,$d/,$2) $(filter $(subst *,%,$2),$d))

GUI_ELM_FILES    = $(wildcard src/gui/*.elm)
BG_ELM_FILES     = $(wildcard src/background/*.elm)
COMMON_ELM_FILES = $(wildcard src/common/*.elm)
JS_FILES         = $(call wildc_recursive, src/, *.js)
HTML_FILES       = $(call wildc_recursive, src/, *.html)
JSON_FILES       = $(call wildc_recursive, src/, *.json)
IMAGE_FILES      = $(wildcard src/gui/images/*)
DIRS 			 = $(sort $(dir $(call wildc_recursive, src/, *)))

all: images elm js json html

dirs   : $(patsubst src%, build%/.dirstamp, $(DIRS))
elm    : dirs build/background/elm-background.js build/gui/elm-gui.js
js     : dirs $(patsubst src/%, build/%, $(JS_FILES))
html   : dirs $(patsubst src/%, build/%, $(HTML_FILES))
json   : dirs $(patsubst src/%, build/%, $(JSON_FILES))
images : dirs $(patsubst src/%, build/%, $(IMAGE_FILES))

%/.dirstamp:
	mkdir $*
	touch $@

build/gui/elm-gui.js: $(GUI_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(GUI_ELM_FILES) --output $@

build/background/elm-background.js: $(BG_ELM_FILES) $(COMMON_ELM_FILES)
	elm-make $(COMMON_ELM_FILES) $(BG_ELM_FILES) --output $@

build/%: src/%
	cp $< $@

clean:
	rm -rf build

.PHONY: all images dirs js elm clean
