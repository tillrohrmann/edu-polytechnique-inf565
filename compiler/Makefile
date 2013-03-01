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

RALL:=runAll
RCOND:=runConditionalTransformation
RDEBRUIJN:=runDeBruijnTransformation
RTYPING:=runTyping
RINTERPRETATION:=runInterpretation
RSTACKMACHINE:=runStackMachine

ALL:= $(RALL) $(RCOND) $(RDEBRUIJN) $(RTYPING) $(RINTERPRETATION) $(RSTACKMACHINE)

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
all: $(ALL)
AUTOGEN_ML=	ml_parser.ml ml_lexer.ml
AUTOGEN_MLI=    ml_parser.mli
AUTOGEN= $(AUTOGEN_ML) $(AUTOGEN_MLI)
ML_FILES=	debug.ml \
			ml_syntax.ml \
			types.ml \
		$(AUTOGEN_ML) \
		helper.ml \
		elaboration.ml \
		interpretation.ml \
		typing.ml \
		stackMachine.ml \
		compiler.ml
		
ML_RUNALL:=runAll.ml
ML_RUNCOND:=runConditionalTransformation.ml
ML_RUNDEBRUIJN:=runDeBruijnTransformation.ml
ML_RUNINTERPRETATION:=runInterpretation.ml
ML_RUNSTACKMACHINE:=runStackMachine.ml
ML_RUNTYPING:=runTyping.ml		

CMO_FILES=	$(ML_FILES:%.ml=%.cmo)
CMX_FILES=  $(ML_FILES:%.ml=%.cmx)
CMX_RALL:= $(CMX_FILES) $(ML_RUNALL:%.ml=%.cmx)
CMX_RCOND:= $(CMX_FILES) $(ML_RUNCOND:%.ml=%.cmx)
CMX_RDEBRUIJN:= $(CMX_FILES) $(ML_RUNDEBRUIJN:%.ml=%.cmx)
CMX_RINTERPRETATION:= $(CMX_FILES) $(ML_RUNINTERPRETATION:%.ml=%.cmx)
CMX_RSTACKMACHINE:= $(CMX_FILES) $(ML_RUNSTACKMACHINE:%.ml=%.cmx)
CMX_RTYPING:=$(CMX_FILES) $(ML_RUNTYPING:%.ml=%.cmx)

$(RALL): $(CMX_RALL) $(AUTOGEN)
	ocamlopt $(CMX_RALL) -o $@
	
$(RCOND): $(CMX_RCOND) $(AUTOGEN)
	ocamlopt $(CMX_RCOND) -o $@
	
$(RDEBRUIJN): $(CMX_RDEBRUIJN) $(AUTOGEN)
	ocamlopt $(CMX_RDEBRUIJN) -o $@
	
$(RINTERPRETATION): $(CMX_RINTERPRETATION) $(AUTOGEN)
	ocamlopt $(CMX_RINTERPRETATION) -o $@
	
$(RSTACKMACHINE): $(CMX_RSTACKMACHINE) $(AUTOGEN)
	ocamlopt $(CMX_RSTACKMACHINE) -o $@
	
$(RTYPING): $(CMX_RTYPING) $(AUTOGEN)
	ocamlopt $(CMX_RTYPING) -o $@

depend: $(AUTOGEN_ML) $(ML_FILES)
	ocamldep $(OCAMLINCLUDES) *.mli *.ml */*.mli */*.ml > depend
clean: 
	rm -f *.cmo *.cmi *.cmx */*.cmi */*.cmo */*.cmx && \
	rm -f *.o $(AUTOGEN_ML) $(AUTOGEN_MLI) compiler depend *~ \
	rm -r *.output
include depend
