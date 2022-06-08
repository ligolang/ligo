include Makefile.coq

HIDE := $(if $(VERBOSE),,@)

Makefile.coq:
	coq_makefile -f _CoqProject -o Makefile.coq

clean::
	$(HIDE)rm .lia.cache Makefile.coq Makefile.coq.conf -f
	$(HIDE)find . -name .\*.aux -type f -delete
