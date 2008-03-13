/* $Id: sybaseopts.c,v 1.1 2004/09/29 04:05:27 wwg Exp $
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
#include <stdio.h>
#include <string.h>
#include <ctpublic.h>

typedef struct {
	char	*name;		/* Option string name */
	int	value;		/* Option enum value */
	char	*atype;		/* Option argument type */
} option_type;

int
main(int argc,char **argv) {
	static option_type options[] = {
		{ "ANSINULL",           CS_OPT_ANSINULL,		"ARG_BOOLEAN"  },
		{ "ANSIPERM",           CS_OPT_ANSIPERM,		"ARG_BOOLEAN"  },
                { "ARITHABORT",		CS_OPT_ARITHABORT,		"ARG_BOOLEAN"  },
                { "ARITHIGNORE",	CS_OPT_ARITHIGNORE,		"ARG_BOOLEAN"  },
                { "AUTHOFF",		CS_OPT_AUTHOFF,			"ARG_CHAR_PTR" },
                { "AUTHON",		CS_OPT_AUTHON,			"ARG_CHAR_PTR" },
                { "CHAINXACTS",		CS_OPT_CHAINXACTS,		"ARG_BOOLEAN"  },
                { "CURCLOSEONXACT",	CS_OPT_CURCLOSEONXACT,		"ARG_BOOLEAN"  },
                { "DATEFIRST",		CS_OPT_DATEFIRST,		"ARG_DAY_OF_WEEK" },
                { "DATEFORMAT",		CS_OPT_DATEFORMAT,		"ARG_DATEFORMAT" },
                { "FIPSFLAG",		CS_OPT_FIPSFLAG,		"ARG_BOOLEAN"  },
                { "FORCEPLAN",		CS_OPT_FORCEPLAN,		"ARG_BOOLEAN"  },
                { "FORMATONLY",		CS_OPT_FORMATONLY,		"ARG_BOOLEAN"  },
                { "GETDATA",		CS_OPT_GETDATA,			"ARG_BOOLEAN"  },
                { "IDENTITYOFF",	CS_OPT_IDENTITYOFF,		"ARG_CHAR_PTR" },
                { "IDENTITYON",		CS_OPT_IDENTITYON,		"ARG_CHAR_PTR" },
                { "ISOLATION",		CS_OPT_ISOLATION,		"ARG_UINT"     },
                { "NOCOUNT",		CS_OPT_NOCOUNT,			"ARG_BOOLEAN"  },
                { "NOEXEC",		CS_OPT_NOEXEC,			"ARG_BOOLEAN"  },
		{ "PARSEONLY",		CS_OPT_PARSEONLY,		"ARG_BOOLEAN"  },
		{ "QUOTED_IDENT",	CS_OPT_QUOTED_IDENT,		"ARG_BOOLEAN"  },
		{ "RESTREES",		CS_OPT_RESTREES,		"ARG_BOOLEAN"  },
		{ "ROWCOUNT",		CS_OPT_ROWCOUNT,		"ARG_UINT"     },
		{ "SHOWPLAN",		CS_OPT_SHOWPLAN,		"ARG_BOOLEAN"  },
		{ "STATS_IO",		CS_OPT_STATS_IO,		"ARG_BOOLEAN"  },
		{ "STATS_TIME",		CS_OPT_STATS_TIME,		"ARG_BOOLEAN"  },
		{ "STR_RTRUNC",		CS_OPT_STR_RTRUNC,		"ARG_BOOLEAN"  },
		{ "TEXTSIZE",		CS_OPT_TEXTSIZE,		"ARG_UINT"     },
		{ "TRUNCIGNORE",	CS_OPT_TRUNCIGNORE,		"ARG_BOOLEAN"  }
	};
	int x;

	for ( x=0; x<sizeof options/sizeof options[0]; ++x ) {
		printf("      (\n");
		printf("        Name        => \"%-18.18s\",\n",options[x].name);
		printf("        Length      => %d,\n",strlen(options[x].name));
		printf("        Sybase_Enum => %u,\n",options[x].value);
		printf("        Argument    => %s\n",options[x].atype);
		printf("      )%s\n",x+1==(sizeof options/sizeof options[0])?"":",");
	}

	return 0;
}

