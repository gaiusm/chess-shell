
SUFFIXES = .c .mod .def .o .obj .lo .a .bnf .flex

MY_DEPS = colors.o chessBoard.o chessShell.o multiprocessor.o stdio.o mailbox.o
CHESSLIBS = colors.o chessBoard.o multiprocessor.o mailbox.o stdio.o
CLIBS = mailbox.o stdio.o multiprocessor.o

OPT=-O0 -fm2-g -g
CHECK=-fsoft-check-all
M2FLAGS=$(OPT) $(CHECK) -fextended-opaque -flibs=pim,iso -Wuninit-variable-checking -fsoft-check-all # -fanalyzer

all-local:  chess-shell$(EXEEXT)

chess-shell$(EXEEXT): $(MY_DEPS)
	mkdir -p chessimages
	cp $(srcdir)/chessimages/tux.svg chessimages/tux.svg
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/chessShell.mod \
            $(CHESSLIBS) -o $@
	gm2 -fm2-whole-program -fiso $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/chessShell.mod -o chess-whole $(CLIBS)

install-exec-local:  chess-shell$(EXEEXT)
	install -m 755 chess-shell$(EXEEXT) $(DESTDIR)$(prefix)/bin

check: force
	rm -f chess.log
	touch chess.log
	for cs in $(srcdir)/regression-tests/*.cs ; do \
           if ./chess-shell $${cs} >> chess.log ; then \
		echo "$${cs}: passed" ; \
	   else \
		echo "$${cs}: failed" ; \
           fi ; \
        done

.mod.o:
	gm2 $(M2FLAGS) -I. -I$(srcdir) -c $< -o $@

force:
