.PHONY: all
all: format test build 

.PHONY: format
format: 
	elm-format --yes src/*.elm

.PHONY: test
test: 
	elm-format --validate src/*.elm

.PHONY: clean
clean: 
	rm -rfv output

.PHONY: output-dir 
output-dir: 
	mkdir -v output 
	cp -r static/* output

output/%.js: src/%.elm
	elm-make $< --output $@


JS_FILES := $(addprefix output/,$(notdir $(wildcard src/*.elm)))
JS_FILES := $(JS_FILES:.elm=.js)


.PHONY: build
build: clean output-dir $(JS_FILES)
