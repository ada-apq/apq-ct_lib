/* $Id: sybase_gentyp.c,v 1.1 2003/09/29 03:42:48 wwg Exp $
 * Copyright (c) 2003, Warren W. Gay VE3WWG
 *
 * Licensed under the ACL (Ada Community License)
 * or
 * GNU Public License 2 (GPL2)
 * 
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
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
