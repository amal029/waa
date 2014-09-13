CCC ?= ocamlopt -annot -g
OCAMLOPTS=-a
BATDIR=`ocamlfind query batteries`
SRC=joplang.ml waa.ml
all: waa

waa:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -UDEBUG" -package batteries -package javalib -linkpkg $(SRC) -o $@


clean:
	rm -rf *cm* *.o *.a waa *mli *.annot *.ini
