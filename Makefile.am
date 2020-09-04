
SUFFIXES = .c .mod .def .o .obj .lo .a .bnf .flex

MY_DEPS = colors.o chessBoard.o chessShell.o multiprocessor.o stdio.o

OPT=-O0 -fm2-g -g
CHECK=-fsoft-check-all
M2FLAGS=$(OPT) $(CHECK) -fextended-opaque -flibs=pim,iso

all-local:  chess-shell$(EXEEXT)

chess-shell$(EXEEXT): $(MY_DEPS)
	gm2 $(M2FLAGS) -I. -I$(srcdir) $(srcdir)/chessShell.mod -o $@

check: force
	for cs in $(srcdir)/regression-tests/*.cs ; do \
           ./chess-shell $${cs} ; \
        done

.mod.o:
	gm2 $(M2FLAGS) -I. -I$(srcdir) -c $< -o $@

force:
