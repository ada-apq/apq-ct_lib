/* $Id: c_sybase.c,v 1.36 2004/09/29 19:10:31 wwg Exp $
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
#include <assert.h>

#include <ctpublic.h>
//#include <sqlca.h>


#define CS_VERSION_125          (CS_INT)12501
#define SY_VERSION CS_VERSION_125	/* Sybase 12.5.x */

/*
 * These values must match type Sy_Row_Info_Type in APQ.Sybase, and
 * be used by the c_sy_results() function below :
 */
#define SY_ROW_INFO_Execution_Failed   0  /* ct_results() call failed */
#define SY_ROW_INFO_No_Results         1  /* Command processed, but no results available */
#define SY_ROW_INFO_Row_Results        2  /* Command processed, and there are row results to fetch */
#define SY_ROW_INFO_Cursor_Results     3  /* Command processed, and there are cursor row results to fetch */
#define SY_ROW_INFO_Info_Results       4  /* Command processed, but no row data, but info exists */
#define SY_ROW_INFO_Compute_Results    5  /* Compute results available */
#define SY_ROW_INFO_Param_Results      6  /* Parameter results available */
#define SY_ROW_INFO_Status_Results     7  /* Status results available */

/*
 * c_sy_get_data return values :
 */
#define SY_GDATA_Get_Data_Failed	0
#define SY_GDATA_Not_Last_Chunk		1
#define SY_GDATA_Last_Chunk		2

/*
 * c_sy_fetch return values :
 */
#define SY_Fetch_Row			20
#define SY_Fetch_End			21
#define SY_Fetch_Failed			22


#define ISOK(r) ( (r == CS_SUCCEED) ? 1 : 0 )

typedef void (*apq_clientmsg_cb)(
	void *conn_type,		/* Connection_Type */
	long msg_layer,			/* Message_Layer */
	long msg_origin,		/* Message_Origin */
	long msg_severity,		/* Message_Severity */
	long msg_number,		/* Message_Number */
	const char *msg_msgstring,	/* Message */
	int msg_length,			/* Message_Length */
	const char *msg_os,		/* OS_Message */
	int msg_oslength		/* OS_Message_Length */
);

typedef void (*apq_servermsg_cb)(
	void *conn_type,		/* Connection_Type */
	long msg_severity,		/* Message_Severity */
	long msg_number,		/* Message_Number */
	long state,			/* Server state */
	long line,			/* Line number */
	const char *svr_name,		/* Server name */
	int svr_name_length,		/* Server name length */
	const char *procname,		/* Procedure name */
	int procname_length,		/* Procedure name length */
	const char *message		/* Server message */
);

static apq_clientmsg_cb apq_clientmsg_proc = 0;
static apq_servermsg_cb apq_servermsg_proc = 0;

#ifdef _WINDOWS

#include <windows.h>
#include <ctype.h>

#define EXPORT __declspec(dllexport)

/*
 * Blinken Windows does not have a strcasecmp() :
 */
static int
strcasecmp(const char *s1, const char *s2) {
	char c1, c2;

	while ( *s1 ) {
		c1 = *s1++;
		c2 = *s2++;
		if ( !c1 || !c2 )
			break;
		if ( islower(c1) )
			c1 = toupper(c1);
		if ( islower(c2) )
			c2 = toupper(c2);
		if ( c1 != c2 )
			return c1 < c2 ? -1 : 1;
	}

	if ( !c1 && !c2 )
		return 0;
	return !c1 ? -1 : 1;
}

int __stdcall
DllMain(HANDLE inst,DWORD reason_called,LPVOID reserved) {
	static inited = 0;
	HINSTANCE NEAR module;

	switch( reason_called ) {
	case DLL_PROCESS_ATTACH :
		if ( !inited++ )
			module = inst;
		break;
	case DLL_THREAD_ATTACH :
		break;
	case DLL_THREAD_DETACH :
		break;
	case DLL_PROCESS_DETACH :
		break;
	default :
		break;
	}

	return TRUE;
}

#else
#define EXPORT
#endif

/*
 * Return the user data pointer (System.Address of APQ.Sybase.Client.Connection_Type) :
 */
EXPORT void *
c_sy_get_userdata(CS_CONNECTION *conn) {
	CS_RETCODE ret;
	void *userdata = 0;

	ret = ct_con_props(conn,CS_GET,CS_USERDATA,&userdata,sizeof userdata,NULL);
	if ( ret != CS_SUCCEED )
#ifdef NDEBUG
		return 0;
#else
		abort();
#endif
	return userdata;
}

/*
 * Register a (Ada) Client Message Callback :
 */ 
EXPORT void
c_sy_reg_clientmsgcb(apq_clientmsg_cb proc) {
	apq_clientmsg_proc = proc;
}

/*
 * Register a (Ada) Server Message Callback :
 */ 
EXPORT void
c_sy_reg_servermsgcb(apq_servermsg_cb proc) {
	apq_servermsg_proc = proc;
}

/*
 * Sybase Message Callback :
 */
static CS_RETCODE CS_PUBLIC
clientmsg_cb(CS_CONTEXT *ctx,CS_CONNECTION *conn,CS_CLIENTMSG *msg) {
	long msg_layer = CS_LAYER(msg->msgnumber);
	long msg_origin = CS_ORIGIN(msg->msgnumber);
	long msg_severity = CS_SEVERITY(msg->msgnumber);
	long msg_number = CS_NUMBER(msg->msgnumber);
	static const char null_msg[] = "";
	int msg_length = strlen(msg->msgstring);
	int msg_oslength = msg->osstringlen;
	const char *msg_os = msg_length > 0 ? msg->osstring : null_msg;

	if ( apq_clientmsg_proc != 0 ) {
		apq_clientmsg_proc(
			c_sy_get_userdata(conn),	/* Connection_Type */
			msg_layer,			/* Message_Layer */
			msg_origin,			/* Message_Origin */
			msg_severity,			/* Message_Severity */
			msg_number,			/* Message_Number */
			msg->msgstring,			/* Message */
			msg_length,			/* Message_Length */
			msg_os,				/* OS_Message */
			msg_oslength			/* OS_Message_Length */
		);
	}

	return CS_SUCCEED;
}

/*
 * Server message callback :
 */
static CS_RETCODE CS_PUBLIC
servermsg_cb(CS_CONTEXT *ctx,CS_CONNECTION *conn,CS_SERVERMSG *msg) {
	long svr_severity = msg->severity;
	long svr_msgnumber = msg->msgnumber;
	long svr_state = msg->state;
	long svr_line = msg->line;
	static const char null_str[] = "";
	int svr_name_length = msg->svrnlen;
	const char *svr_name = svr_name_length > 0 ? msg->svrname : null_str;
	int svr_proclen = msg->proclen;
	const char *svr_procname = svr_proclen > 0 ? msg->proc : null_str;

	if ( apq_servermsg_proc != 0 ) {
		apq_servermsg_proc(
			c_sy_get_userdata(conn),	/* Connection_Type */
			svr_severity,			/* Message Severity */
			svr_msgnumber,			/* Message Number */
			svr_state,			/* Server State */
			svr_line,			/* Line number */
			svr_name,			/* Server name */
			svr_name_length,		/* Length */
			svr_procname,			/* Procedure name */
  			svr_proclen,			/* Length */
			msg->text			/* Server message */
		);
	}

	return CS_SUCCEED;
}

EXPORT void
c_sy_pad(char *buffer,int curlen,int size) {
	int x = curlen;
	
	for ( ; x < size; ++x )
		buffer[x] = ' ';
}

/*
 * Convert to Sybase bits to APQ bits :
 */
static int
c_sy_to_column_status(int cs_value) {
	static struct CS_TAB {
		CS_INT	syb_bit;
		int	apq_bit;
	} table[] = {
		{ CS_CANBENULL,		1 },
		{ CS_HIDDEN,		2 },
		{ CS_IDENTITY,		4 },
		{ CS_KEY,		8 },
		{ CS_VERSION_KEY,	16 },
		{ CS_TIMESTAMP,		32 },
		{ CS_UPDATABLE,		64 },
		{ CS_UPDATECOL,		128 },
		{ CS_RETURN,		256 },
		{ 0,			0 }
	};
	int x;
	int r = 0;

	for ( x=0; table[x].apq_bit != 0; ++x )
		if ( cs_value & table[x].syb_bit )
			r |= table[x].apq_bit;
	return r;
}

/*
 * Describe a column :
 */
EXPORT int
c_sy_describe(
	CS_COMMAND *cmd,		/* Input : Command structure */
	int item,			/* Input : Column index, starting from 1 */
	char *name,			/* Output : Name buffer */
	int *name_size,			/* In/Out:  Name buffer size / returned length */
	int *datatype,			/* Output : Column data type */
	int *status,			/* Output : Column status bits */
	int *max_length,		/* Output : The maximum possible data length */
	int *scale,			/* Output : The column's scale */
	int *prec			/* Output : The column's precision */
) {
	CS_INT itemx = (CS_INT)item;	/* Column index */
	CS_DATAFMT fmt;			/* Returned data format info */
	CS_RETCODE ret;

	assert(cmd != NULL);
	assert(itemx > 0);

	ret = ct_describe(cmd,itemx,&fmt);
	if ( ret != CS_SUCCEED )
		return 0;		/* Call failed */

	/*
	 * Column name, if any (else '')
	 */
	if ( *name_size < CS_MAX_NAME )
		abort();

	strncpy(name,fmt.name,fmt.namelen);
	c_sy_pad(name,fmt.namelen,*name_size);
	*name_size = fmt.namelen;

	/*
	 * Pass back the column's format :
	 */
	*datatype = fmt.datatype;

	/*
	 * Column status bits :
	 */
	*status = c_sy_to_column_status(fmt.status);
	
	/*
	 * Maximum possible data length :
	 */
	*max_length = fmt.maxlength;

	/*
	 * Scale, precision :
	 */
	*scale = fmt.scale;
	*prec  = fmt.precision;

	return 1;			/* Call succeeded */
}

/*
 * Get Error Information :
 * 
 * typedef struct _sqlca {
 * 	char	sqlcaid[8];			-- "SQLCA"
 * 	long	sqlcabc;			-- Ignored
 * 	long	sqlcode;			-- SQLCODE
 * 	struct {
 * 		long	sqlerrml;		-- Length of message in sqlerrmc[]
 * 		char	sqlerrmc[256];		-- Null terminated message of length sqlerrml
 * 	} sqlerrm;
 * 	char	sqlerrp[8];			-- Null terminated name of stored procedure etc.
 * 	long	sqlerrd[6];			-- sqlerrd[2] has # of rows affected (else ignored)
 * 	char	sqlwarn[8];			-- Warnings, see below
 * 	char	sqlext[8];			-- Ignored
 * 
 * WARNINGS:
 * 	sqlwarn[0]	when blank, all other warnings are blank, else 'W'
 * 	sqlwarn[1]	when 'W', then at least one column value was truncated, else blank
 * 	sqlwarn[2]	when 'W', then at least one null value was eliminated from a function argument
 * 	sqlwarn[3]	when 'W', then some but not all values were bound (only for CS_ANSI_BINDS)
 * 	sqlwarn[4]	when 'W', then a dynamic update/delete statement did not have a where clause
 * 	sqlwarn[5]	when 'W', then a server conversion or truncation error occurred
 */
EXPORT int
c_sy_get_error(
	CS_CONNECTION *conn,		/* Input:	Connection */
	long *sqlcode,			/* Output:	SQLCODE */
	char *errmp,			/* Output:	Error message buffer */
	int  *errmp_size,		/* In/Out:	errmp buffer size / Message length */
	char *sqlerrp,			/* Output:	sqlerrp buffer */
	int  sqlerrp_size,		/* Input:	sqlerrp buffer size */
	long *rows_affected,		/* Output:	# of rows affected */
	char *sqlwarn			/* Output:	sqlwarn[5] buffer */
) {
	int msgbuf_size = *errmp_size;	/* Save maximum buffer size */
	CS_INT x;
	CS_RETCODE ret;
	int rc;

	assert(conn != NULL);
	assert(sqlcode != NULL);
	assert(errmp != NULL);
	assert(errmp_size != NULL);
	assert(sqlerrp != NULL);
	assert(rows_affected != NULL);
	assert(sqlwarn != NULL);

//	lcl_sqlIca.sqlerrd[2] = 0;


	// TODO: I took this from Sybase's cspublic.h
	// There should be a similar #define in FreeTDS but I can't find it for now
	CS_INT SQLCA_TYPE=(CS_INT) 4703;

	for ( x=1; ; ++x ) {
		ret = ct_diag(conn,CS_GET,SQLCA_TYPE,x,errmp);
		if ( ret != CS_SUCCEED )
			break;
	}

	if ( ret == CS_NOMSG )
		ret = CS_SUCCEED;

	rc = ISOK(ret);

	ret = ct_diag(conn,CS_CLEAR,SQLCA_TYPE,CS_UNUSED,NULL);
	assert(ret == CS_SUCCEED);

//	if ( rc != 0 ) {
//		*sqlcode = lcl_sqlca.sqlcode;
//		strncpy(errmp,lcl_sqlca.sqlerrm.sqlerrmc,msgbuf_size-1)[msgbuf_size-1] = 0;
//		*errmp_size = strlen(lcl_sqlca.sqlerrm.sqlerrmc);		/* Return message size */
//		strncpy(sqlerrp,lcl_sqlca.sqlerrp,sqlerrp_size);
//		c_sy_pad(sqlerrp,strlen(lcl_sqlca.sqlerrp),sqlerrp_size);	/* Right pad to length */
//		*rows_affected = lcl_sqlca.sqlerrd[2];				/* # rows affected */
//		memcpy(sqlwarn,lcl_sqlca.sqlwarn,6);				/* 6 Warning flags */
//
//		if ( !lcl_sqlca.sqlcode ) {					/* If no error code.. */
//			errmp[0] = ' ';
//			errmp[1] = 0;
//			*errmp_size = 1;					/* clear the message */
//		}
//	}

	return rc;
}

/*
 * Return the CS_CONTEXT used by the given CS_CONNECTION :
 */
EXPORT CS_CONTEXT *
c_sy_context_of(CS_CONNECTION *conn) {
	CS_CONTEXT *ctx = NULL;
	CS_RETCODE ret;

	ret = ct_con_props(conn,CS_GET,CS_PARENT_HANDLE,&ctx,CS_UNUSED,NULL);
	if ( ret == CS_SUCCEED )
		return ctx;
	return NULL;
}

/*
 * Return the CS_CONNECTION used by the given CS_COMMAND :
 */
EXPORT CS_CONNECTION *
c_sy_connection_of(CS_COMMAND *cmd) {
	CS_CONNECTION *conn = NULL;
	CS_RETCODE ret;

	ret = ct_cmd_props(cmd,CS_GET,CS_PARENT_HANDLE,&conn,CS_UNUSED,NULL);
	if ( ret == CS_SUCCEED )
		return conn;
	return NULL;
}

/*
 * Send the queued SQL to the server (flush) :
 */
EXPORT int
c_sy_send(CS_COMMAND *cmd) {
	CS_RETCODE ret;

	ret = ct_send(cmd);
	return ret == CS_SUCCEED ? 1 : 0;
}

/*
 * Allocate a Sybase context :
 */
EXPORT CS_CONTEXT *
c_sy_alloc_context(void) {
	CS_CONTEXT *ctx = NULL;
	CS_RETCODE ret;

	if ( (ret = cs_ctx_alloc(SY_VERSION,&ctx)) != CS_SUCCEED )
		return NULL;

	if ( (ret = ct_init(ctx,SY_VERSION)) != CS_SUCCEED ) {
errxit:		cs_ctx_drop(ctx);
		return NULL;
	}

	if ( (ret = ct_callback(ctx,NULL,CS_SET,CS_CLIENTMSG_CB,clientmsg_cb)) != CS_SUCCEED )
		goto errxit;

	if ( (ret = ct_callback(ctx,NULL,CS_SET,CS_SERVERMSG_CB,servermsg_cb)) != CS_SUCCEED )
		goto errxit;

	return ctx;
}

/*
 * Free a Sybase context :
 */
EXPORT CS_CONTEXT *
c_sy_free_context(CS_CONTEXT *ctx) {
	CS_RETCODE ret;

	ret = ct_exit(ctx,CS_UNUSED);  /* Ignore result? */    /* DO WE NEED THIS??? *FIXME* */

	ret = cs_ctx_drop(ctx);
	if ( ret == CS_SUCCEED )
		return NULL;	/* Success */
	else	return ctx;	/* Failed: This can fail if the LANG value is not supported by Sybase */
}

/*
 * Free a Sybase connection :
 */
EXPORT CS_CONNECTION *
c_sy_free_connection(CS_CONNECTION *conn) {
	CS_RETCODE ret;

	ret = ct_con_drop(conn);
	if ( ret == CS_SUCCEED )
      		return NULL;	/* Success */
	else	return conn;	/* Failed */
}

/*
 * Allocate a Sybase connection :
 */
EXPORT CS_CONNECTION *
c_sy_alloc_connection(CS_CONTEXT *ctx,void *userdata) {
	CS_CONNECTION *conn = NULL;
	CS_RETCODE ret;
	int sy_true = CS_TRUE;

	ret = ct_con_alloc(ctx,&conn);
	if ( ret != CS_SUCCEED )
		goto xit;		/* Failed */

#if 1	/* LATER: Test to see if this is needed when using callbacks */
	/* Set CS_EXTRA_INF to get SQLCODE */
	ret = ct_con_props(conn,CS_SET,CS_EXTRA_INF,&sy_true,CS_UNUSED,NULL);
	if ( ret != CS_SUCCEED )
		goto xit;		/* Failed */
#endif

	/* Set CS_HIDDEN_KEYS so that we can do random cursor fetches */
	ret = ct_con_props(conn,CS_SET,CS_HIDDEN_KEYS,&sy_true,CS_UNUSED,NULL);
	if ( ret != CS_SUCCEED )
		goto xit;		/* Failed */

	/* Link connection back to Ada Connection_Type object address */
	ret = ct_con_props(conn,CS_SET,CS_USERDATA,&userdata,sizeof userdata,NULL);
	if ( ret != CS_SUCCEED )
		goto xit;

xit:	if ( ret != CS_SUCCEED && conn != NULL )
		c_sy_free_connection(conn);

	return ret == CS_SUCCEED ? conn : NULL;	/* Success if not null */
}

/*
 * Set the userid for the connection :
 */
EXPORT int
c_sy_set_userid(CS_CONNECTION *conn,char *userid) {
	CS_RETCODE ret;

	ret = ct_con_props(conn,CS_SET,CS_USERNAME,userid,CS_NULLTERM,NULL);
	return ISOK(ret);
}

/*
 * Set the password for the connection :
 */
EXPORT int
c_sy_set_passwd(CS_CONNECTION *conn,char *passwd) {
	CS_RETCODE ret;

	ret = ct_con_props(conn,CS_SET,CS_PASSWORD,passwd,CS_NULLTERM,NULL);
	return ISOK(ret);
}

/*
 * Set the database name for the connection :
 */
EXPORT int
c_sy_set_database(CS_CONNECTION *conn,char *database) {
	CS_RETCODE ret;

	ret = ct_con_props(conn,CS_SET,CS_APPNAME,database,CS_NULLTERM,NULL);
	return ISOK(ret);
}

/*
 * Specify the host name :
 */
EXPORT int
c_sy_set_hostname(CS_CONNECTION *conn,char *host) {
	CS_RETCODE ret;

	ret = ct_con_props(conn,CS_SET,CS_HOSTNAME,host,CS_NULLTERM,NULL);
	return ISOK(ret);
}

/*
 * Connect to the database engine (instance) :
 */
EXPORT int
c_sy_connect(CS_CONNECTION *conn,char *instance) {
	CS_RETCODE ret;

	ret = ct_connect(conn,(CS_CHAR *)instance,strlen(instance));
	return ISOK(ret);
}

/*
 * Disconnect from the server :
 */
EXPORT int
c_sy_disconnect(CS_CONNECTION *conn) {
	CS_RETCODE ret;

	ret = ct_close(conn,CS_UNUSED);
	return ISOK(ret);
}   

/*
 * Issue an SQL command to the server :
 */
EXPORT CS_COMMAND *
c_sy_exec(CS_CONNECTION *conn,char *sql) {
	CS_COMMAND *cmd = NULL;
	CS_RETCODE ret;
	int rc;

	assert(conn != NULL);
	assert(sql != NULL );

	ret = ct_cmd_alloc(conn,&cmd);
	if ( ret != CS_SUCCEED )
		return NULL;			/* FAILED */

	ret = ct_command(cmd,CS_LANG_CMD,sql,CS_NULLTERM,CS_UNUSED);
	if ( (rc = ISOK(ret)) != 0 )
		rc = c_sy_send(cmd);		/* Send what we have queued */

	if ( !rc ) {				/* Did anything fail? */
		if ( cmd != NULL ) {		/* Was there anything allocated? */
			ct_cmd_drop(cmd);	/* Yes, release what we allocated */
			cmd = NULL;
		}
		return NULL;			/* Indicate failure */
	}

	return cmd;				/* Return CS_COMMAND pointer for success */
}

/*
 * Get standard results :
 *
 *	(1)	res == CS_CMD_SUCCEED
 *	(2)	res == CS_CMD_DONE
 *		=> returns True
 *	anything else:
 *		=> returns False
 */
EXPORT int
c_sy_isdone(CS_COMMAND *cmd) {
	CS_INT res = 0;
	CS_RETCODE ret;

	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED ) {
		switch ( res ) {
		case CS_CMD_SUCCEED :
			break;			/* Expected intermediate result */
		case CS_CMD_DONE :
			return 1;
		default :
			return 0;		/* Failed! This value is not expected! */
		}
	}

	return 0;
}

/*
 * Get standard results :
 *
 *	(1)	res == CS_CMD_SUCCEED
 *	(2)	res == CS_CMD_DONE
 *      (3)     ret == CS_END_RESULTS
 *		=> returns True
 *	anything else:
 *		=> returns False
 */
EXPORT int
c_sy_isend(CS_COMMAND *cmd) {
	CS_INT res = 0;
	CS_RETCODE ret;

	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED ) {
		switch ( res ) {
		case CS_CMD_SUCCEED :
			break;			/* This was expected */
		case CS_CMD_DONE :
			break;			/* This was expected */
		default :
			return 0;		/* Failed! This value is not expected! */
		}
	}
	return ret == CS_END_RESULTS ? 1 : 0;	/* Succeeded if we got CS_END_RESULTS */
}

/*
 * For debugging use only :
 */
EXPORT int
c_sy_dbg_results(CS_COMMAND *cmd) {
	CS_INT res = 0;
	CS_RETCODE ret;

	fprintf(stderr,"C_SY_DBG_RESULTS:\n");
	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED ) {
		fprintf(stderr,"  RES=%d\n",(int)res);
	}
	fprintf(stderr,"End: ret=%d.\n",(int)ret);
	fflush(stderr);

	return 0;
}

/*
 * Just eat any pending results :
 */
EXPORT void
c_sy_eatresults(CS_COMMAND *cmd) {
	CS_INT res = 0;
	CS_RETCODE ret;

	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED )
		;				/* Just eat results */
}

/*
 * Open and launch an SQL cursor :
 */
EXPORT CS_COMMAND *
c_sy_cursor(CS_CONNECTION *conn,char *cursor_name,int namelen,char *sql,int for_update) {
	CS_COMMAND *cmd = NULL;
	CS_RETCODE ret;

	assert(conn != NULL);
	assert(cursor_name != NULL);
	assert(namelen > 0);
	assert(sql != NULL );

	ret = ct_cmd_alloc(conn,&cmd);
	if ( ret != CS_SUCCEED )
		goto err;

	ret = ct_cursor(cmd,CS_CURSOR_DECLARE,cursor_name,(CS_INT)namelen,sql,CS_NULLTERM,
 		for_update ? CS_FOR_UPDATE : CS_READ_ONLY);
	if ( ret != CS_SUCCEED )
		goto err;

	ret = ct_cursor(cmd,CS_CURSOR_ROWS,NULL,CS_UNUSED,NULL,CS_UNUSED,(CS_INT)1);
	if ( ret != CS_SUCCEED )
		goto err;
	
	ret = ct_cursor(cmd,CS_CURSOR_OPEN,NULL,CS_UNUSED,NULL,CS_UNUSED,CS_UNUSED);
	if ( ret != CS_SUCCEED )
		goto err;

	ret = ct_send(cmd);
	if ( ret != CS_SUCCEED )
		goto err;

	if ( !c_sy_isdone(cmd) ) {	/* Check declare cursor */
		c_sy_eatresults(cmd);	/* Eat any other possible pending results */
		goto err;		/* Declare cursor failed */
	}

	if ( !c_sy_isdone(cmd) )	/* Check open cursor */
		goto err;		/* Open cursor failed */

	return cmd;			/* Success */

	/*
	 * Failed :
	 */
err:	if ( cmd != NULL )
		ct_cmd_drop(cmd);
	return NULL;
}

/*
 * Close the SQL cursor :
 */
EXPORT int
c_sy_close(CS_COMMAND *cmd) {
	CS_RETCODE ret;
	CS_INT res;

	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED )	/* Should normally only execute once */
		;
	assert(ret == CS_END_RESULTS || ret == CS_CANCELED );	/* Should return CS_END_RESULTS */

	ret = ct_cursor(cmd,CS_CURSOR_CLOSE,NULL,CS_UNUSED,NULL,CS_UNUSED,CS_DEALLOC);
	if ( ret == CS_SUCCEED ) {
		ct_send(cmd);
		if ( c_sy_isdone(cmd) )				/* Check the CS_CMD_DONE status */
			return 1;				/* Full success */
	}

	return 0;	/* Failed */
}

/*
 * Free an allocated SQL command :
 *
 * NOTE:	This is trickier than it looks. Normally,
 *		ct_cmd_drop() is only called by an application
 *		at the right moment, after the normal sequence
 *		of Sybase calls. However, the APQ Query_Type
 *		object can be destroyed or cleared before any
 *		rows are fetched (particularly if finalized due
 *		to an exception.)
 *
 *		Here, if the ct_cmd_drop() command fails, it
 *		then performs a cancel on the current cmd's
 *		struct. Then it retries the ct_cmd_drop(), 
 *		which normally does the trick.
 *
 *		If this routine is not dropping when it should
 *		be, then perhaps there are some other sequences
 *		of events that need to be handled.
 */
EXPORT CS_COMMAND *
c_sy_release(CS_COMMAND *cmd) {
	CS_RETCODE ret;
	CS_INT res;

	assert(cmd != NULL);

	while ( (ret = ct_results(cmd,&res)) == CS_SUCCEED )	/* Flush any pending result codes, if any */
		;						/* Just ignore (dump) the res codes */

	ret = ct_cmd_drop(cmd);					/* Drop the CS_COMMAND struct */
	if ( ret != CS_SUCCEED ) {				/* This can fail.. */
		ret = ct_cancel(NULL,cmd,CS_CANCEL_ALL);	/* It needs a cancel.. */
		if ( ret == CS_SUCCEED )			/* If cancelled, then.. */
			ret = ct_cmd_drop(cmd);			/* Now drop should work.. */
	}

	if ( ISOK(ret) != 0 )
		return NULL;			/* Structure was freed */
	else	return cmd;			/* Something was wrong */
}

/*
 * Return digested results summary :
 *
 * NOTES:
 *
 *	1.	Sybase does not return information about the #
 *		of rows to be returned.
 *
 *	2.	When row data is returned, the number of columns
 *		is returned via the cols parameter.
 */
EXPORT int
c_sy_results(CS_COMMAND *cmd,int *cols) {
	CS_RETCODE ret;
	CS_INT res = 0;
	CS_INT num = 0;

	ret = ct_results(cmd,&res);
	if ( ret != CS_SUCCEED ) {
		switch ( ret ) {
		case CS_END_RESULTS :		/* This shouldn't happen */
			assert(1 != 0);
			abort();
		case CS_FAIL :
                        return SY_ROW_INFO_Execution_Failed;
		case CS_PENDING :
//		case CS_BUSY :
		default :
			fprintf(stderr,"Yikes! ret=%d, Line %d in %s!\n",(int)ret,__LINE__,__FILE__);
		}
		abort();
	}

	switch ( res ) {
	case CS_CMD_FAIL :			/* Server error, and no results */
		*cols = 0;
		return SY_ROW_INFO_Execution_Failed;

	case CS_CMD_DONE :			/* Command processed, but no results applicable */
		*cols = 0;
		return SY_ROW_INFO_No_Results;

	case CS_CMD_SUCCEED :			/* Command processed, but there are no results */
		*cols = 0;
		return SY_ROW_INFO_No_Results;

	case CS_COMPUTE_RESULT :		/* A single row of computed results are available */
		*cols = 0;
		return SY_ROW_INFO_Compute_Results;

	case CS_PARAM_RESULT :			/* A single row of return parameter values */
		*cols = 0;
		return SY_ROW_INFO_Param_Results;

	case CS_STATUS_RESULT :			/* A single row containing stored procedure return status */
		*cols = 0;
		return SY_ROW_INFO_Status_Results;

	case CS_ROW_RESULT :			/* Zero or more rows of tabular data available */

		/*
		 * Determine how many columns were returned :
		 */
		ret = ct_res_info(cmd,CS_NUMDATA,&num,CS_UNUSED,NULL);
		assert(ret == CS_SUCCEED);
		*cols = (int)num;

		return SY_ROW_INFO_Row_Results;

	case CS_CURSOR_RESULT :			/* Zero or more rows of tabular data available */

		/*
		 * Determine how many columns were returned :
		 */
		ret = ct_res_info(cmd,CS_NUMDATA,&num,CS_UNUSED,NULL);
		assert(ret == CS_SUCCEED);
		*cols = (int)num;

		return SY_ROW_INFO_Cursor_Results;

	case CS_COMPUTEFMT_RESULT :		/* Info, but no fetchable results */
	case CS_ROWFMT_RESULT :			/* ditto */
	case CS_MSG_RESULT :			/* ditto */
	case CS_DESCRIBE_RESULT :		/* ditto */
		*cols = 0;
		return SY_ROW_INFO_Info_Results;

	default :
		fprintf(stderr,"Yikes! res=%d, Line %d in %s!\n",(int)res,__LINE__,__FILE__);
	}

	abort();
	return 13;	/* To satisfy the compiler only : code does not get here */
}

/*
 * Get data for the application :
 */
EXPORT int
c_sy_get_data(CS_COMMAND *cmd,int item,void *buffer,int buflen,int *outlen) {
	CS_INT lcl_outlen = 0;
	CS_RETCODE ret;

	ret = ct_get_data(cmd,(CS_INT)item,buffer,(CS_INT)buflen,&lcl_outlen);
	switch ( ret ) {
	case CS_FAIL :
		return SY_GDATA_Get_Data_Failed;
	case CS_SUCCEED :
		*outlen = (int)lcl_outlen;
		return SY_GDATA_Not_Last_Chunk;
	case CS_END_ITEM :
	case CS_END_DATA :
		*outlen = (int)lcl_outlen;
		return SY_GDATA_Last_Chunk;
	default :
		return SY_GDATA_Get_Data_Failed;
	}
}

EXPORT int
c_sy_fetch(CS_COMMAND *cmd) {
	CS_INT rows_read;
	CS_RETCODE ret;

	ret = ct_fetch(cmd,CS_UNUSED,CS_UNUSED,CS_UNUSED,&rows_read);

	switch ( ret ) {
	case CS_SUCCEED :
		return SY_Fetch_Row;
	case CS_END_DATA :
		return SY_Fetch_End;
	default :
		c_sy_isdone(cmd);		/* Just eat status: we know it failed */
		return SY_Fetch_Failed;
	}
}

/*
 * Convert a data item in a result to a string :
 */
EXPORT int
c_sy_to_string(CS_COMMAND *cmd,int item,int src_fmt,char *dest,int *dest_len) {
	CS_CONTEXT *ctx = c_sy_context_of(c_sy_connection_of(cmd));
	CS_DATAFMT src_format, dst_format;
	CS_RETCODE ret;
	char src_buf[257];
	void *srcp = src_buf;
	int src_len = 0;
	CS_INT res_len = 0;
	CS_DATETIME dt;
	CS_DATEREC drec;

	/*
	 * First get the value to convert :
	 */
	if ( c_sy_get_data(cmd,item,src_buf,sizeof src_buf,&src_len) != SY_GDATA_Last_Chunk )
		return 0;		/* We failed */

	if ( !src_len ) {
		*dest_len = 0;		/* The value is NULL */
		return 1;		/* Return Successful */
	}

	srcp = src_buf;			/* Initially assume the source buffer */

	switch ( src_fmt ) {
//	case CS_TIME_TYPE :
//		dt.dtdays = 0;
//		dt.dttime = *(CS_INT *)src_buf;	/* Grab the time component */
//		ret = cs_dt_crack(ctx,CS_DATETIME_TYPE,&dt,&drec);
//		if ( ret != CS_SUCCEED )
//			return 0;		/* Failed */
//		sprintf(dest,"%02d:%02d:%02d",
//			(int)drec.datehour,
//			(int)drec.dateminute,
//			(int)drec.datesecond);
//		*dest_len = strlen(dest);
//		return 1;			/* Successfully converted */
//	case CS_DATE_TYPE :
//		dt.dtdays = *(CS_INT *)src_buf;	/* Grab the date component */
//		dt.dttime = 0;			/* Set the time component to zero */
//		ret = cs_dt_crack(ctx,CS_DATETIME_TYPE,&dt,&drec);
//		if ( ret != CS_SUCCEED )
//			return 0;		/* Failed */
//		sprintf(dest,"%04d-%02d-%02d",
//			(int)drec.dateyear,
//			(int)drec.datemonth+1,
//			(int)drec.datedmonth);
//		*dest_len = strlen(dest);
//		return 1;			/* Successfully converted */
	case CS_DATETIME_TYPE :
	case CS_DATETIME4_TYPE :
		ret = cs_dt_crack(ctx,src_fmt,srcp,&drec);
		if ( ret != CS_SUCCEED )
			return 0;		/* Failed */
		if ( src_fmt == CS_DATETIME_TYPE )
			sprintf(dest,"%04d-%02d-%02d %02d:%02d:%02d.%03d",
				(int)drec.dateyear,
				(int)drec.datemonth+1,
				(int)drec.datedmonth,
				(int)drec.datehour,
				(int)drec.dateminute,
				(int)drec.datesecond,
				(int)drec.datemsecond);
		else	sprintf(dest,"%04d-%02d-%02d %02d:%02d",
				(int)drec.dateyear,
				(int)drec.datemonth+1,
				(int)drec.datedmonth,
				(int)drec.datehour,
				(int)drec.dateminute);
		*dest_len = strlen(dest);
		return 1;			/* Successfully converted */
	default :
		break;
	}

	src_format.datatype = src_fmt;
	src_format.maxlength = src_len;
	src_format.locale = NULL;

	dst_format.datatype = CS_CHAR_TYPE;
	dst_format.maxlength = *dest_len;
	dst_format.locale = NULL;
	dst_format.format = CS_FMT_UNUSED;

	dst_format.scale = 0;
	dst_format.precision = 0;

	ret = cs_convert(ctx,&src_format,src_buf,&dst_format,dest,&res_len);
	*dest_len = (int)res_len;

	return ret == CS_SUCCEED ? 1 : 0;
}

/*
 * Cancel the results of the current command.
 */
EXPORT int
c_sy_cancel(CS_COMMAND *cmd) {
	CS_RETCODE ret;
	CS_INT status;

	ret = ct_cancel(NULL,cmd,CS_CANCEL_ALL);

	if ( ret == CS_SUCCEED ) {
		ret = ct_cmd_props(cmd,CS_GET,CS_CUR_STATUS,&status,CS_UNUSED,(CS_INT *)NULL);

		if ( ret == CS_SUCCEED ) {
			if ( (status & CS_CURSTAT_OPEN) == CS_CURSTAT_OPEN )
				return 2;
		}
	return 1;	/* Succeeded (but no cursor info) */
	}

	return 0;	/* Failed */
}

EXPORT int
c_sy_bool_option(CS_CONNECTION *conn,int opt,int arg) {
	CS_RETCODE ret;
	CS_INT option = opt;
	CS_BOOL b;
	
	if ( arg != 0 )
		b = CS_TRUE;
	else	b = CS_FALSE;

	ret = ct_options(conn,CS_SET,option,&b,CS_UNUSED,NULL);
	return ret == CS_SUCCEED ? 1 : 0;
}

EXPORT int
c_sy_uint_option(CS_CONNECTION *conn,int opt,unsigned arg) {
	CS_RETCODE ret;
	CS_INT option = opt;
	unsigned u = arg;
	
	ret = ct_options(conn,CS_SET,option,&u,CS_UNUSED,NULL);
	return ret == CS_SUCCEED ? 1 : 0;
}

EXPORT int
c_sy_dow_option(CS_CONNECTION *conn,int opt,int arg) {
	CS_RETCODE ret;
	CS_INT option = opt;
	static CS_INT dow[] = {
		CS_OPT_SUNDAY, CS_OPT_MONDAY, CS_OPT_TUESDAY, CS_OPT_WEDNESDAY, CS_OPT_THURSDAY,
		CS_OPT_FRIDAY, CS_OPT_SATURDAY
	};
	
	ret = ct_options(conn,CS_SET,option,&dow[arg],CS_UNUSED,NULL);
	return ret == CS_SUCCEED ? 1 : 0;
}

EXPORT int
c_sy_format_option(CS_CONNECTION *conn,int opt,int arg) {
	CS_RETCODE ret;
	CS_INT option = opt;
	static CS_INT fmt[] = {
		CS_OPT_FMTMDY, CS_OPT_FMTDMY, CS_OPT_FMTYMD, CS_OPT_FMTYDM, CS_OPT_FMTMYD, CS_OPT_FMTDYM
	};
	
	ret = ct_options(conn,CS_SET,option,&fmt[arg],CS_UNUSED,NULL);
	return ret == CS_SUCCEED ? 1 : 0;
}

EXPORT int
c_sy_string_option(CS_CONNECTION *conn,int opt,char * arg) {
	CS_RETCODE ret;
	CS_INT option = opt;
	
	ret = ct_options(conn,CS_SET,option,arg,CS_NULLTERM,NULL);
	return ret == CS_SUCCEED ? 1 : 0;
}

/* End $Source: /cvsroot/apq/apq/c_sybase.c,v $ */
