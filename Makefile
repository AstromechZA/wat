JS_FILES := $(addprefix output/,$(notdir $(wildcard src/*.elm)))
JS_FILES := $(JS_FILES:.elm=.js)
HTML_FILES := $(addprefix output/,$(notdir $(wildcard static/*.html)))
CSS_FILES := $(addprefix output/,$(notdir $(wildcard static/*.css)))

.PHONY: all
all: output $(HTML_FILES) $(JS_FILES) $(CSS_FILES)

.PHONY: clean
clean: 
	rm -rfv output

output: 
	mkdir -v output 

output/%.html: static/%.html 
	cp $< $@

output/%.css: static/%.css 
	cp $< $@

output/%.js: src/%.elm
	elm-format --yes $<
	elm-make $< --output $@
