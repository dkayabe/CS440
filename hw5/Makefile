all: lexer.mll parser.mly types.ml
	ocamllex lexer.mll
	ocamlyacc -v parser.mly
#	ocamlc -g -o test types.ml parser.mli lexer.ml parser.ml unify.ml context.ml typecheck.ml test.ml
	ocamlc -g -o miniml types.ml parser.mli lexer.ml parser.ml print.ml env.ml interp.ml context.ml unify.ml typecheck.ml main.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f miniml

full-clean: clean
	rm -f lexer.ml
	rm -f parser.ml
	rm -f parser.mli
