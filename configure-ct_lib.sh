#!/usr/bin/env bash
# Setup the apq-ct_lib.ads file from the ct_lib's Sources.





###########
# Testing #
###########


# Test if the input is empty
# Prints [ok] and [fail] and set the exit code accordingly
test_is_set(){
	if [[ "$1" = "" ]]
	then
		echo "[fail]";
		exit -1;
	else
		echo "[ok]";
	fi;
}




###################
# Data retrieving #
###################


###############
# Field Types #
###############


# Print into the standart output the list of ct_lib codes for data types
get_field_type_codes(){
	cp src-in/ct_lib_gentyp.c "$TMP_PATH/"
	echo $CC "${TMP_PATH}/ct_lib_gentyp.c" -o "${TMP_PATH}/ct_lib_gentyp" $CT_LIB_CFLAGS -I"${TMP_PATH}" > lol
	
	$CC "${TMP_PATH}/ct_lib_gentyp.c" -o "${TMP_PATH}/ct_lib_gentyp" $CT_LIB_CFLAGS -I"${TMP_PATH}" && ./"$TMP_PATH/ct_lib_gentyp" |  sort -k1,1n
}



######################
# Connection Options #
######################

get_connection_options(){
	cp src-in/ct_lib_opts.c "$TMP_PATH/"
	$CC "${TMP_PATH}/ct_lib_opts.c" -o "${TMP_PATH}/ct_lib_opts" $CT_LIB_CFLAGS -I"${TMP_PATH}" && ./"$TMP_PATH/ct_lib_opts" 
}


#############################
# The ct_lib Linker Options #
#############################

# Print each linker option pragma...
get_linker_options(){
	for i in $CT_LIB_LIBS
	do
		echo "	pragma Linker_Options( \"$i\" );";
	done;
}


