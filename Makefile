CCC ?= ocamlopt -g -S -inline 20 -nodynlink -annot -ccopt -O3 -ccopt -mtune=native -ccopt -flto
JAVALIBDIR=`ocamlfind query javalib`
SRCO=myReachDef.ml live_ref.ml
SRCN=joplang.ml wcma.ml
all: wcma live_ref

wcma:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib,deriving -package sawja -package batteries -package javalib \
	-linkpkg $(SRCN) -o $@
live_ref:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib,deriving -package sawja -package batteries -package javalib \
	-linkpkg $(SRCO) -o $@


clean:
	rm -rf *.cm* *.o *.a wcma waa *.annot *.log live_ref *.class *.s *.ini*
