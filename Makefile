CCC ?= ocamlopt -g -S -inline 20 -nodynlink -annot -ccopt -O3 -ccopt -mtune=native -ccopt -flto
JAVALIBDIR=`ocamlfind query javalib`
SRCO=myReachDef.ml live_ref.ml
SRCN=wcma.ml
LIBSRC=joplang.ml libWcma.ml
LIB=libWcma.cmxa

all: live_ref libWcma.cmxa wcma

wcma:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I "."	\
	$(LIB) -package extlib,deriving -package sawja -package		\
	batteries -package javalib -linkpkg $(SRCN) -o $@

libWcma.cmxa:
	ocamlfind $(CCC) -c -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib,deriving -package sawja -package batteries -package javalib \
	-linkpkg $(LIBSRC)
	$(CCC) -a joplang.cmx libWcma.cmx -o $@

joplang.cmxa:
	ocamlfind $(CCC) -c -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib,deriving -package sawja -package batteries -package javalib \
	-linkpkg -c $(LIBSRC)
	$(CCC) -a joplang.cmx -o $@

live_ref:
	ocamlfind $(CCC) -pp "camlp4o pa_macro.cmo -DDEBUG" -package extlib,deriving -package sawja -package batteries -package javalib \
	-linkpkg $(SRCO) -o $@


clean:
	rm -rf *.cm* *.o *.a wcma waa *.annot *.log live_ref *.class *.s *.ini*
