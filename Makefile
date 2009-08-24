# Makefile for the AW_Lib
#
# @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 

PROJECT_FILES=apq-ct_lib_c.gpr apq-ct_lib.gpr
GPR_FILES=apq-ct_lib.gpr apq-ct_lib_c.gpr


INCLUDE_FILES=src*/*

#OUTPUT_NAME is the name of the compiled library.
ifeq ($(OS), Windows_NT)
	OUTPUT_NAME=apq-ct_libhelp.dll
else
	OUTPUT_NAME=libapq-ct_libhelp.so
endif



include Makefile.include


pre_libs: c_libs

pos_libs:


c_objs:
	make -C src-c/

c_objs-clean:
	make -C src-c/ clean



apq-ct_lib.ads:
	make -C extras

apq-ct_lib.ads-clean:
	make -C extras clean
	rm -f src/apq-ct_lib.ads


c_libs: apq-ct_lib.ads c_objs
	cd lib && gcc -shared  ../obj-c/c_ct_lib.o -o $(OUTPUT_NAME)  -lct


extra_clean: apq-ct_lib.ads-clean c_objs-clean 
	@rm -f obj-c/* lib/*
	gnatclean -P ${projectFile}
	@echo "All clean"


