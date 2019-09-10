BINDIR=./

CC = gcc
# CFLAGS = -ansi -g
CFLAGS = -ansi -O2

all: acc.o matrice.o liste.o acc

acc: acc.o matrice.o liste.o
	$(CC) $(CFLAGS) $^ -o acc -lm




clean::
	rm -f acc
	rm -f *.o
	rm -f *.*~
	rm -f *~
