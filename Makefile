CCC ?= ocamlopt -g -annot -ccopt -O3 -ccopt -mtune=native -ccopt -flto
JAVALIBDIR=`ocamlfind query javalib`
SRCO=live_ref.ml
SRCN=joplang.ml wcma.ml
all: wcma live_ref

wcma:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib -package sawja -package batteries -package javalib \
	-linkpkg $(SRCN) -o $@
live_ref:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib -package sawja -package batteries -package javalib \
	-linkpkg $(SRCO) -o $@


clean:
	rm -rf *.cm* *.o *.a wcma waa *.annot *.ini *.log live_ref *.class
