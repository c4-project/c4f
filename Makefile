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

.PHONY: install
install:
	dune build @install
	opam install .

.PHONY: test
test:
	dune runtest --force

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

#
# Commands for making the act paper
# (These aren't run by default)
#

paper: paper/paper.pdf

paper/paper.pdf: paper/paper.tex
	cd paper && latexmk -pdf paper

.PHONY: clean-paper
clean-paper:
	cd paper && latexmk -C paper
