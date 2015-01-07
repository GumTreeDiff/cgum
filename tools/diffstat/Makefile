LIBNAMES=xml-light
OCAMLLIBS=str.cma unix.cma $(LIBNAMES:=.cma)

diffstat: diffstat.ml
	ocamlc -g -o diffstat $(OCAMLLIBS) diffstat.ml
