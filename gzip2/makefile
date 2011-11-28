all: pgzip

pgzip:		gzip.o zip.o deflate.o trees.o bits.o util.o threads.o
		gcc -lpthread gzip.o zip.o deflate.o trees.o bits.o util.o threads.o -o pgzip

gzip.o:		gzip.c
		gcc -c gzip.c

zip.o:		zip.c
		gcc -c zip.c

deflate.o:	deflate.c
		gcc -c deflate.c

trees.o:	trees.c
		gcc -c trees.c

bits.o:		bits.c
		gcc -c bits.c

util.o:		util.c
		gcc -c util.c

threads.o:	threads.c
		gcc -c threads.c 

clean:		
		rm *.o
