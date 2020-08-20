CAMLBUILD := ocamlbuild -use-ocamlfind -classic-display

CMA := src/nurpawiki.cma
CMXA := src/nurpawiki.cmxa
CMXS := src/nurpawiki.cmxs

TARGETS := $(CMA)
ifneq ($(shell command -v ocamlopt),)
  TARGETS += $(CMXA)
  ifneq ($(wildcard $(shell ocamlc -where)/dynlink.cmxa),)
    TARGETS += $(CMXS)
  endif
endif

.PHONY: all install

all: $(TARGETS) META

$(CMA): src/version.ml
	$(CAMLBUILD) $@

$(CMXA): src/version.ml
	$(CAMLBUILD) $@

$(CMXS): src/version.ml
	$(CAMLBUILD) $@

NWIKI_VER=$(shell cat VERSION)

src/version.ml: src/version.ml.in VERSION
	echo $(NWIKI_VER)
	sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" $< > $@

META: META.in VERSION
	sed -e "s|%_NURPAWIKI_VERSION_%|$(NWIKI_VER)|g" $< > $@

clean:
	$(CAMLBUILD) -clean
	-rm -Rf _build META src/version.ml

install:
	ocamlfind install nurpawiki META $(foreach T,$(TARGETS),_build/$(T) $(if $(findstring .cmxa,$(T)),_build/$(T:.cmxa=.a)))
