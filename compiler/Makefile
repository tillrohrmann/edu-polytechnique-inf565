OCAMLPREFIX=
OCAMLC=         $(OCAMLPREFIX)ocamlc
OCAMLOPT=       $(OCAMLPREFIX)ocamlopt
OCAMLYACC=      $(OCAMLPREFIX)ocamlyacc -v
OCAMLLEX=       $(OCAMLPREFIX)ocamllex
OCAMLDEP=       $(OCAMLPREFIX)ocamldep
OCAMLINCLUDES=
OCAMLFLAGS=     -warn-error -a $(OCAMLINCLUDES)
OCAMLC=         $(OCAMLPREFIX)ocamlc
OCAMLOPT=       $(OCAMLPREFIX)ocamlopt

OUTPUT:=runner

%.ml: %.mll
	$(OCAMLLEX) $*.mll
%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml
%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $*.ml
all: $(OUTPUT)
AUTOGEN_ML=	ml_parser.ml ml_lexer.ml
AUTOGEN_MLI=    ml_parser.mli
AUTOGEN= $(AUTOGEN_ML) $(AUTOGEN_MLI)
ML_FILES=	ml_syntax.ml \
			types.ml \
		$(AUTOGEN_ML) \
		helper.ml \
		elaboration.ml \
		interpretation.ml \
		typing.ml \
		stackMachine.ml \
		compiler.ml \
		runner.ml
CMO_FILES=	$(ML_FILES:%.ml=%.cmo)
CMX_FILES=      $(ML_FILES:%.ml=%.cmx)
$(OUTPUT): $(CMX_FILES) $(AUTOGEN)
	ocamlopt $(CMX_FILES) -o $(OUTPUT)

depend: $(AUTOGEN_ML) $(ML_FILES)
	ocamldep $(OCAMLINCLUDES) *.mli *.ml */*.mli */*.ml > depend
clean: 
	rm -f *.cmo *.cmi *.cmx */*.cmi */*.cmo */*.cmx && \
	rm -f *.o $(AUTOGEN_ML) $(AUTOGEN_MLI) compiler depend *~ \
	rm -r *.output
include depend
