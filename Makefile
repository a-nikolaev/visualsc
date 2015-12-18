
EXECS = visualsc

all: $(EXECS)

visualsc: common.ml sc.ml layout.ml opt.ml draw.ml
	ocamlfind ocamlopt -g -package cairo2 -package str -linkpkg -o $@ $^

clean:
	-rm -f *.cmx *.o *.cmi $(EXECS)
