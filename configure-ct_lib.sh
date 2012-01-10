#!/usr/bin/env bash
# Setup the apq-mysql.ads file from the ct_lib's Sources.





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

generate_field_type_c_chunk(){
	HDRFILE="${CT_LIB_INCLUDE_PATH}/mysql_com.h"
	sed <"$HDRFILE" -n '/enum_field_types/,/};/p' | sed 's|enum||;s|enum_field_types||;s|[{};]||g;s|,|\
|g;s|[ 	]*||g' | sed '/^$/d;s|=[0-9]*||g' \
        | while read NAME ; do
                echo "  { \"$NAME\", $NAME },"
        done
        echo "  { 0, 0 }"
}


# Print into the standart output the list of ct_lib codes for data types
get_field_type_codes(){
	generate_field_type_c_chunk > "$TMP_PATH/mysql_type_codes.h"
	cp src-in/mysql_gentyp.c "$TMP_PATH/"
	$CC "${TMP_PATH}/mysql_gentyp.c" -o "${TMP_PATH}/mysql_gentyp" $CT_LIB_CFLAGS $CT_LIB_LIBS -I"${TMP_PATH}" && ./"$TMP_PATH/mysql_gentyp" |  sort -k1,1n
}



######################
# Connection Options #
######################

generate_conection_options_c_chunk(){

	HDRFILE="${CT_LIB_INCLUDE_PATH}/mysql.h"
	for NAME in `sed -n  -e '/mysql_option/,/^};$/p' -e '/};/q' $HDRFILE | sed -e 's/,//g' | grep -v "}" | grep -v "{" | grep -v "enum mysql_option"`
	do
                echo "  { \"$NAME\", $NAME },"
	done
        echo "  { \"none\", 3105 }," # a huge number, also my birthdat in the Br syntax TODO :: verify if the NONE value is really necessary
	echo "{0,0}" # exit criteria
}

get_connection_options(){
	generate_conection_options_c_chunk > "$TMP_PATH/mysql_option_codes.h"
	cp src-in/mysql_genop.c "$TMP_PATH/"
	$CC "${TMP_PATH}/mysql_genop.c" -o "${TMP_PATH}/mysql_genop" $CT_LIB_CFLAGS $CT_LIB_LIBS -I"${TMP_PATH}" && ./"$TMP_PATH/mysql_genop" |  sort -k1,1n
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


