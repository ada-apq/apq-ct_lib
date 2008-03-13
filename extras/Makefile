all: sybase_gentyp sybaseopts
	./work/sybaseopts > work/sybopts.tmp
	./work/sybase_gentyp | ./prep_sybase "$(SY_LIBS)" > ../src/apq-sybase.ads



sybase_gentyp:
	gcc sybase_gentyp.c -o work/sybase_gentyp

sybaseopts:
	gcc sybaseopts.c -o work/sybaseopts



clean:
	rm -f work/*
