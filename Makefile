CCC ?= ocamlc -annot -g
JAVALIBDIR=`ocamlfind query javalib`
SRCO=joplang.ml waa.ml
SRCN=joplang.ml wcma.ml
all: waa wcma

wcma:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib -package sawja -package batteries -package javalib \
	-linkpkg $(SRCN) -o $@
waa:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib -package sawja -package batteries -package javalib \
	-linkpkg $(SRCO) -o $@


clean:
	rm -rf *.cm* *.o *.a wcma waa *.annot *.ini
