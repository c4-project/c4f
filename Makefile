# As suggested by Issuu:
#
# https://engineering.issuu.com/2018/11/20/our-current-ocaml-best-practices-part-1
all: build

.PHONY: clean
clean:
	dune clean

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune runtest --force
