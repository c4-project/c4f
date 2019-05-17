# As suggested by Issuu:
#
# https://engineering.issuu.com/2018/11/20/our-current-ocaml-best-practices-part-1
all: build

.PHONY: clean build install test fmt doc open-doc api-doc open-api-doc

clean:
	dune clean

build:
	dune build

install:
	opam install .

test:
	dune runtest --force

fmt:
	dune build @fmt --auto-promote

doc:
	cd docs && make html

open-doc:
	open docs/_build/html/index.html

api-doc:
	dune build @doc

open-api-doc:
	open _build/default/_doc/_html/index.html

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
