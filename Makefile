run: first_order.cmo gcl.cmo lexer.cmo parser.cmo run.cmo
	ocamlc first_order.cmo gcl.cmo lexer.cmo parser.cmo run.cmo -o run

first_order.cmo: first_order.ml
	ocamlc -c first_order.ml

gcl.cmo: gcl.ml
	ocamlc -c gcl.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer.cmo: parser.cmo lexer.ml
	ocamlc -c lexer.ml

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.ml
	ocamlc -c parser.mli

parser.cmo : parser.cmi parser.ml
	ocamlc -c parser.ml

run.cmo: run.ml
	ocamlc -c run.ml

clean:
	rm run lexer.ml parser.ml *.cmi *.cmo
