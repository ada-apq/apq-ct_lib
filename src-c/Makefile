

all: c_sybase.o


clean:
	rm ../obj-c/*

.c.o:
	cd ../obj-c && gcc -fPIC -I../src-c -c ../src-c/$< -o $*.o
	

