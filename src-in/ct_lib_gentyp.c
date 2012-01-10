/****************************************************************************/
/*                          APQ DATABASE BINDINGS                           */
/*                                                                          */
/*                              A P Q - CT_LIB 				    */
/*                                                                          */
/*                                                                          */
/*         Copyright (C) 2002-2007, Warren W. Gay VE3WWG                    */
/*         Copyright (C) 2007-2009, Ada Works Project                       */
/*                                                                          */
/*                                                                          */
/* APQ is free software;  you can  redistribute it  and/or modify it under  */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  APQ is distributed in the hope that it will be useful, but WITH-  */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with APQ;  see file COPYING.  If not, write  */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a special exception,  if other files  instantiate  generics from this */
/* unit, or you link  this unit with other files  to produce an executable, */
/* this  unit  does not  by itself cause  the resulting  executable  to  be */
/* covered  by the  GNU  General  Public  License.  This exception does not */
/* however invalidate  any other reasons why  the executable file  might be */
/* covered by the  GNU Public License.                                      */
/****************************************************************************/

/* #define NDEBUG */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ctpublic.h>

int
main(int argc,char **argv) {
	static struct {
		CS_INT	syb_type;
		char	*name;
	} table[] = {
		{ CS_CHAR_TYPE,		"CHAR" },
		{ CS_BINARY_TYPE,	"BINARY" },
		{ CS_LONGCHAR_TYPE,	"LONGCHAR" },
		{ CS_LONGBINARY_TYPE,	"LONGBINARY" },
		{ CS_TEXT_TYPE,		"TEXT" },
		{ CS_IMAGE_TYPE,	"IMAGE" },
		{ CS_TINYINT_TYPE,	"TINYINT" },
		{ CS_SMALLINT_TYPE,	"SMALLINT" },
		{ CS_INT_TYPE,		"INT" },
		{ CS_REAL_TYPE,		"REAL" },
		{ CS_FLOAT_TYPE,	"FLOAT" },
		{ CS_BIT_TYPE,		"BIT" },
		{ CS_DATETIME_TYPE,	"DATETIME" },
		{ CS_DATETIME4_TYPE,	"DATETIME4" },
		{ CS_MONEY_TYPE,	"MONEY" },
		{ CS_MONEY4_TYPE,	"MONEY4" },
		{ CS_NUMERIC_TYPE,	"NUMERIC" },
		{ CS_DECIMAL_TYPE,	"DECIMAL" },
		{ CS_VARCHAR_TYPE,	"VARCHAR" },
		{ CS_VARBINARY_TYPE,	"VARBINARY" },
		{ CS_LONG_TYPE,		"LONG" },
		{ CS_SENSITIVITY_TYPE,	"SENSITIVITY" },
		{ CS_BOUNDARY_TYPE,	"BOUNDARY" },
		{ CS_VOID_TYPE,		"VOID" },
		{ CS_USHORT_TYPE,	"USHORT" },
		{ CS_UNICHAR_TYPE,	"UNICHAR" },
		{ CS_ILLEGAL_TYPE,	NULL }
	};
	int x;	

	for ( x=0; table[x].name != NULL; ++x )
		printf("%d Type_%s\n",(int)table[x].syb_type,table[x].name);
	return 0;
}

/* End $Source: /cvsroot/apq/apq/sybase_gentyp.c,v $ */
