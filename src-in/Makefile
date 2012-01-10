all: ct_lib_gentyp ct_lib_opts
	./work/ct_lib_opts > work/ctlibopts.tmp
	./work/ct_lib_gentyp | ./prep_ct_lib "$(SY_LIBS)" > ../src/apq-ct_lib.ads


ct_lib_gentyp:
	gcc ct_lib_gentyp.c -o work/ct_lib_gentyp

ct_lib_opts:
		gcc ct_lib_opts.c -o work/ct_lib_opts


clean:
	rm -f work/*
