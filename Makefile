CCC ?= ocamlopt -annot -g
OCAMLOPTS=-a
BATDIR=`ocamlfind query batteries`
SRC=waa.ml
all: waa

waa:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package batteries -package javalib -linkpkg $(SRC) -o $@


clean:
	rm -rf *cm* *o *a *mli *.annot waa *.ini
