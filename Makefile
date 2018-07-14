.PHONY: all
all: test build 

.PHONY: format
format: 
	elm-format --yes *.elm tests/*.elm

.PHONY: test
test: 
	elm-format --validate *.elm

.PHONY: clean
clean: 
	rm -rfv output

.PHONY: build
build: clean
	mkdir output
	cp -r static/* output/
	elm-make bytes.elm --output output/bytes.js
	elm-make jsonformat.elm --output output/jsonformat.js
	elm-make subnet.elm --output output/subnet.js
