
CFLAGS=-g -O0
M2FLAGS=-g -O0
M2FLAGS=-g -O0 -fm2-g
M2FLAGS=-g -fm2-g -fsoft-check-all -fsources
M2WHOLE=$(M2FLAGS) -fm2-whole-program

whole: force
	gm2 $(M2WHOLE) chessShell.mod multiprocessor.o

all: chessShell.o chessBoard.o multiprocessor.o colors.o stdio.o
	gm2 $(M2FLAGS) chessShell.mod multiprocessor.o

chessShell.o:  chessShell.mod
	gm2 $(M2FLAGS) -c chessShell.mod

chessBoard.o:  chessBoard.mod
	gm2 $(M2FLAGS) -c chessBoard.mod

colors.o:  colors.mod
	gm2 $(M2FLAGS) -c colors.mod

multiprocessor.o:  multiprocessor.c
	gcc -c -g $(CFLAGS) -o $@ $<

stdio.o:  stdio.c
	gcc -c -g $(CFLAGS) -o $@ $<

clean: force
	$(RM) *.o a.out

force:
