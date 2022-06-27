EMACS  = emacs
OUTPUT = compile-commands.elc

.PHONY: all
all: $(OUTPUT)

.el.elc:
	$(EMACS) -Q --batch -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(OUTPUT)
