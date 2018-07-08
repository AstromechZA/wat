.PHONY: test
test: 
	elm-format --validate

.PHONY: clean
clean: 
	rm -rfv output

.PHONY: build
build: clean
	mkdir output
	cp -r static/* output
	elm-make bytes.elm --output static/bytes.html

.PHONY: all
all: test build 
