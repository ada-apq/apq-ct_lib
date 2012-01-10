------------------------------------------------------------------------------
--                                                                          --
--                          APQ DATABASE BINDINGS                           --
--                                                                          --
--                              A P Q - SYBASE				    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Warren W. Gay VE3WWG                    --
--         Copyright (C) 2007-2009, Ada Works Project                       --
--         Copyright (C) 2009-2012, KOW Framework Project                   --
--                                                                          --
--                                                                          --
-- APQ is free software;  you can  redistribute it  and/or modify it under  --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  APQ is distributed in the hope that it will be useful, but WITH-  --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with APQ;  see file COPYING.  If not, write  --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------


with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

use Ada.Exceptions;

package body APQ.CT_Lib is

	No_Warnings : constant String(1..6) := "      ";

	-- 
	-- FINALIZATION FOR SQLCA_TYPE
	-- 
	procedure Finalize(SQLCA : in out SQLCA_Type) is
	begin
		if SQLCA.Sqlerrm /= null then
			Free(SQLCA.Sqlerrm);		-- Free error message string
		end if;
	end Finalize;


	--
	-- CLEAR SQLCA ERROR(S)
	--
	procedure Clear_SQLCA(SQLCA : in out SQLCA_Type) is
	begin
		if SQLCA.Sqlerrm /= null then
			Free(SQLCA.Sqlerrm);
		end if;
		SQLCA.Sqlcode := 0;
		SQLCA.Sqlerrp := "        ";
		SQLCA.Sqlwarn := No_Warnings;
	end Clear_SQLCA;


	--
	-- FREE SYBASE CONTEXT
	--
	procedure CT_Lib_Free_Context(Ctx : in out CT_Lib_Context_Type) is
		function c_ct_lib_free_context(Ctx : CT_Lib_Context_Type) return CT_Lib_Context_Type;
		pragma import(C,c_ct_lib_free_context,"c_ct_lib_free_context");
	begin
		Ctx := c_ct_lib_free_context(Ctx);
	end CT_Lib_Free_Context;


	--
	-- FREE SYBASE CONNECTION
	--
	procedure CT_Lib_Free_Connection(Conn : in out CT_Lib_Conn_Type) is
		function c_ct_lib_free_connection(Conn : CT_Lib_Conn_Type) return CT_Lib_Conn_Type;
		pragma import(C,c_ct_lib_free_connection,"c_ct_lib_free_connection");
	begin
		Conn := c_ct_lib_free_connection(Conn);
	end CT_Lib_Free_Connection;


	--
	-- SET HOSTNAME TO USE FOR CONNECTION
	--
	function CT_Lib_Set_Hostname(Conn : CT_Lib_Conn_Type; Host : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_ct_lib_set_hostname(Conn : CT_Lib_Conn_Type; host : System.Address) return Return_Status;
		pragma import(C,c_ct_lib_set_hostname,"c_ct_lib_set_hostname");
		C_Host :	char_array_access;
		A_Host :	System.Address := System.Null_Address;
		RC :		Return_Status;
	begin
		C_String(To_String(Host),C_Host,A_Host);
		RC := c_ct_lib_set_hostname(Conn,A_Host);
		if C_Host /= null then
			Free(C_Host);
		end if;
		return RC /= 0;
	end CT_Lib_Set_Hostname;


	--
	-- SET SYBASE USERID
	--
	function CT_Lib_Set_Userid(Conn : CT_Lib_Conn_Type; Userid : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_ct_lib_set_userid(Conn : CT_Lib_Conn_Type; userid : System.Address) return Return_Status;
		pragma import(C,c_ct_lib_set_userid,"c_ct_lib_set_userid");
		C_Userid :	char_array_access;
		A_Userid :	System.Address;
		RC :		Return_Status;
	begin
		C_String(To_String(Userid),C_Userid,A_Userid);
		RC := c_ct_lib_set_userid(Conn,A_Userid);
		if C_Userid /= null then
			Free(C_Userid);
		end if;
		return RC /= 0;
	end CT_Lib_Set_Userid;


	--
	-- SET THE PASSWORD TO USE
	--
	function CT_Lib_Set_Passwd(Conn : CT_Lib_Conn_Type; Passwd : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_ct_lib_set_passwd(Conn : CT_Lib_Conn_Type; passwd : System.Address) return Return_Status;
		pragma import(C,c_ct_lib_set_passwd,"c_ct_lib_set_passwd");
		C_Passwd :	char_array_access;
		A_Passwd :	System.Address;
		RC :		Return_Status;
	begin
		C_String(To_String(Passwd),C_Passwd,A_Passwd);
		RC := c_ct_lib_set_passwd(Conn,A_Passwd);
		if C_Passwd /= null then
			Free(C_Passwd);
		end if;
		return RC /= 0;
	end CT_Lib_Set_Passwd;


	--
	-- SET THE DATABASE NAME TO USE
	--
	function CT_Lib_Set_Database(Conn : CT_Lib_Conn_Type; Database : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_ct_lib_set_database(Conn : CT_Lib_Conn_Type; database : System.Address) return Return_Status;
		pragma import(C,c_ct_lib_set_database,"c_ct_lib_set_database");
		C_Db :	char_array_access;
		A_Db :	System.Address;
		RC :	Return_Status;
	begin
		C_String(To_String(Database),C_Db,A_Db);
		RC := c_ct_lib_set_database(Conn,A_Db);
		if C_Db /= null then
			Free(C_Db);
		end if;
		return RC /= 0;
	end CT_Lib_Set_Database;


	--
	-- CONNECT TO THE SYBASE SERVER
	--
	function CT_Lib_Connect(Conn : CT_Lib_Conn_Type; Instance : String) return Boolean is
		use Interfaces.C.Strings;
		function c_ct_lib_connect(Conn : CT_Lib_Conn_Type; instance : System.Address) return Return_Status;
		pragma import(C,c_ct_lib_connect,"c_ct_lib_connect");
		C_Instance :	char_array_access;
		A_Instance :	System.Address;
		RC :		Return_Status;
	begin
		C_String(Instance,C_Instance,A_Instance);
		RC := c_ct_lib_connect(Conn,A_Instance);
		if C_Instance /= null then
			Free(C_Instance);
		end if;
		return RC /= 0;
	end CT_Lib_Connect;


	--
	-- DISCONNECT FROM THE SYBASE SERVER
	--
	function CT_Lib_Disconnect(Conn : CT_Lib_Conn_Type) return Boolean is
		function c_ct_lib_disconnect(Conn : CT_Lib_Conn_Type) return Return_Status;
		pragma import(C,c_ct_lib_disconnect,"c_ct_lib_disconnect");
		RC : Return_Status;
	begin
		RC := c_ct_lib_disconnect(Conn);
		return RC /= 0;
	end CT_Lib_Disconnect;


	--
	-- ALLOCATE A SYBASE CS_COMMAND, SEND SQL TO SERVER, AND EXECUTE IT
	--
	function CT_Lib_Exec(Conn : CT_Lib_Conn_Type; SQL : String) return CT_Lib_Cmd_Type is
		use Interfaces.C;
		function c_ct_lib_exec(conn : CT_Lib_Conn_Type; SQL : System.Address) return CT_Lib_Cmd_Type;
		pragma import(C,c_ct_lib_exec,"c_ct_lib_exec");
		C_SQL : char_array := To_C(SQL);
	begin
		return c_ct_lib_exec(Conn,C_SQL'Address);
	end CT_Lib_Exec;


	--
	-- OPEN AN SQL CURSOR
	--
	function CT_Lib_Open_Cursor(
		Conn :		CT_Lib_Conn_Type;		-- Sybase connection
		SQL :		String;			-- SQL text
		Cursor_Name :	System.Address;		-- Start of cursor name
		Name_Length :	Natural;		-- Length of cursor name
		For_Update :	Boolean			-- True if cursor is for update
	) return CT_Lib_Cmd_Type is
		use Interfaces.C;

		function c_ct_lib_cursor(Conn : CT_Lib_Conn_Type; Name : System.Address; Name_Len : Int_Type; SQL : System.Address; For_Update : Int_Type)
			return CT_Lib_Cmd_Type;
		pragma import(C,c_ct_lib_cursor,"c_ct_lib_cursor");

		C_SQL :		char_array := To_C(SQL);
		Update :	Int_Type := 0;
	begin
		if For_Update then
			Update := 1;
		end if;
		return c_ct_lib_cursor(Conn,Cursor_Name,Int_Type(Name_Length),C_SQL'Address,Update);
	end CT_Lib_Open_Cursor;


	--
	-- CLOSE & DEALLOCATE SQL CURSOR (BUT DO NOT DEALLOCATE CT_Lib_CMD_TYPE)
	--
	function CT_Lib_Close_Cursor(Cmd : CT_Lib_Cmd_Type) return Boolean is
		function c_ct_lib_close(Conn : CT_Lib_Cmd_Type) return Int_Type;
		pragma import(C,c_ct_lib_close,"c_ct_lib_close");
	begin
		return c_ct_lib_close(Cmd) /= 0;
	end CT_Lib_Close_Cursor;


	--
	-- SYBASE CT_RESULTS() CALL :
	--
	procedure CT_Lib_Results(Cmd : CT_Lib_Cmd_Type; Results : out Result_Type; Columns : out Natural) is
		function c_ct_lib_results(Cmd : CT_Lib_Cmd_Type; Cols : System.Address) return Result_Type;
		pragma import(C,c_ct_lib_results,"c_ct_lib_results");

		Num_Cols : Int_Type := 0;
	begin
		Results := c_ct_lib_results(Cmd,Num_Cols'Address);
		Columns := Natural(Num_Cols);
	end CT_Lib_Results;


	--
	-- Special Result check for CS_CMD_DONE
	--
	function CT_Lib_Is_Done(Cmd : CT_Lib_Cmd_Type) return Boolean is
		function c_ct_lib_isdone(Cmd : CT_Lib_Cmd_Type) return Int_Type;
		pragma import(C,c_ct_lib_isdone,"c_ct_lib_isdone");
	begin
		return c_ct_lib_isdone(Cmd) /= 0;
	end CT_Lib_Is_Done;


	--
	-- Special Result check for CS_CMD_DONE & CS_END_RESULTS
	--
	function CT_Lib_Is_End(Cmd : CT_Lib_Cmd_Type) return Boolean is
		function c_ct_lib_isend(Cmd : CT_Lib_Cmd_Type) return Int_Type;
		pragma import(C,c_ct_lib_isend,"c_ct_lib_isend");
	begin
		return c_ct_lib_isend(Cmd) /= 0;
	end CT_Lib_Is_End;


	--
	-- Print a debug trace of Sybase results to stderr
	--
	procedure CT_Lib_Debug_Results(Cmd : CT_Lib_Cmd_Type) is
		procedure c_ct_lib_dbg_results(Cmd : CT_Lib_Cmd_Type);
		pragma import(C,c_ct_lib_dbg_results,"c_ct_lib_dbg_results");
	begin
		c_ct_lib_dbg_results(Cmd);
	end CT_Lib_Debug_Results;


	--
	-- Cancel pending results
	--
	function CT_Lib_Cancel(Cmd : CT_Lib_Cmd_Type) return CT_Lib_Cancel_Type is
		function c_ct_lib_cancel(Cmd : CT_Lib_Cmd_Type) return Int_Type;
		pragma import(C,c_ct_lib_cancel,"c_ct_lib_cancel");

		R : Int_Type;
	begin
		R := c_ct_lib_cancel(Cmd);
		case R is
			when 0 =>
				return Cancel_Failed;
			when 1 =>
				return Cancel_Succeeded;
			when 2 =>
				return Cancel_With_Open_Cursor;
			when others =>
				raise Program_Error;
		end case;
	end CT_Lib_Cancel;



	--
	-- DESCRIBE THE COLUMNS FROM A RESULT :
	--
	function CT_Lib_Describe(Cmd : CT_Lib_Cmd_Type; Columns : Positive) return CT_Lib_Columns_Ptr is
		procedure c_ct_lib_describe(
			Cmd :		in	CT_Lib_Cmd_Type;
			Item :		in	Int_Type;
			Name :		in	System.Address;
			Name_Size :	in	System.Address;
			Data_Type :	in	System.Address;
			Status :	in	System.Address;
			Max_Length :	in	System.Address;
			Scale :		in	System.Address;
			Precision :	in	System.Address
		);
		pragma import(C,c_ct_lib_describe,"c_ct_lib_describe");

		D : CT_Lib_Columns_Ptr := new CT_Lib_Columns_Array(1..Column_Index_Type(Columns));
	begin

		for X in D'Range loop
			D(X).Name_Length := D(X).Name'Length;
			c_ct_lib_describe(
				Cmd		=> Cmd,
				Item		=> Int_Type(X),
				Name		=> D(X).Name(1)'Address,
				Name_Size	=> D(X).Name_Length'Address,
				Data_Type	=> D(X).Data_Type'Address,
				Status		=> D(X).Status'Address,
				Max_Length	=> D(X).Max_Length'Address,
				Scale		=> D(X).Scale'Address,
				Precision	=> D(X).Precision'Address
			);
			D(X).Str_Value := null;
		end loop;
		return D;

	end CT_Lib_Describe;


	--
	-- SYBASE CT_GET_DATA() :
	--
	procedure CT_Lib_Get_Data(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; Buf : System.Address; Len : Natural; Out_Len : out Natural; RC : out Get_Data_Type) is
		function c_ct_lib_get_data(Cmd : CT_Lib_Cmd_Type; Item : Int_Type; Buf : System.Address; Len : Int_Type; Out_Len : System.Address) return Get_Data_Type;
		pragma import(C,c_ct_lib_get_data,"c_ct_lib_get_data");
		Rtn : Int_Type;
	begin
		RC := c_ct_lib_get_data(Cmd,Int_Type(Item),Buf,Int_Type(Len),Rtn'Address);
		Out_Len := Natural(Rtn);
	end CT_Lib_Get_Data;


	--
	-- GET CHAR_TYPE DATA :
	--
	function CT_Lib_Get_Char_Data(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; Max_Length : Natural ) return String_Ptr is
		use Ada.Strings.Unbounded;

		Ret_Code :	Get_Data_Type;
		Char_Buf :	String(1..Max_Length);
		Char_Len :	Natural;
		Char_Val :	Unbounded_String;
	begin

		loop
			CT_Lib_Get_Data(Cmd,Item,Char_Buf(1)'Address,Char_Buf'Length,Char_Len,Ret_Code);
			exit when Ret_Code = Get_Data_Failed;
			if Char_Len > 0 then
				Append(Char_Val,Char_Buf(1..Char_Len));
			end if;
			exit when Ret_Code = Last_Chunk;
		end loop;

		if Ret_Code /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY50: Unable to fetch all character data (CT_Lib_Get_Char_Data).");
		end if;

		if Length(Char_Val) < 1 then
			return null;		-- The value is null
		else
			return new String'(To_String(Char_Val));
		end if;

	end CT_Lib_Get_Char_Data;


	--
	-- GET 2 BYTE INTEGER VALUE
	--
	procedure CT_Lib_Get_U2(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out U2_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		U2 :	U2_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,U2'Address,2,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY51: Unable to get all data (CT_Lib_Get_U2).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 2);
			V := U2;
		else
			V := 0;
		end if;
	end CT_Lib_Get_U2;


	--
	-- GET 1 BYTE INTEGER VALUE
	--
	procedure CT_Lib_Get_I1(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out I1_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I1 :	I1_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,I1'Address,1,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY52: Unable to get all data (CT_Lib_Get_I1).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 1);
			V := I1;
		else
			V := 0;
		end if;
	end CT_Lib_Get_I1;


	--
	-- GET 2 BYTE INTEGER VALUE
	--
	procedure CT_Lib_Get_I2(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out I2_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I2 :	I2_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,I2'Address,2,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY53: Unable to get all data (CT_Lib_Get_I2).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 2);
			V := I2;
		else
			V := 0;
		end if;
	end CT_Lib_Get_I2;


	--
	-- GET 4 BYTE INTEGER VALUE
	--
	procedure CT_Lib_Get_I4(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out I4_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I4 :	I4_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,I4'Address,4,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY54: Unable to get all data (CT_Lib_Get_I4).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 4);
			V := I4;
		else
			V := 0;
		end if;
	end CT_Lib_Get_I4;


	--
	-- GET 8 BYTE INTEGER VALUE
	--
	procedure CT_Lib_Get_I8(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out I8_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I8 :	I8_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,I8'Address,8,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY55: Unable to get all data (CT_Lib_Get_I8).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 8);
			V := I8;
		else
			V := 0;
		end if;
	end CT_Lib_Get_I8;


	--
	-- GET 4 BYTE REAL VALUE
	--
	procedure CT_Lib_Get_R4(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out R4_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		R4 :	R4_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,R4'Address,4,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY56: Unable to get all data (CT_Lib_Get_R4).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 4);
			V := R4;
		else
			V := 0.0;
		end if;
	end CT_Lib_Get_R4;


	--
	-- GET 8 BYTE REAL VALUE
	--
	procedure CT_Lib_Get_R8(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; V : out R8_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		R8 :	R8_Type;
	begin
		CT_Lib_Get_Data(Cmd,Item,R8'Address,8,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY57: Unable to get all data (CT_Lib_Get_R8).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 8);
			V := R8;
		else
			V := 0.0;
		end if;
	end CT_Lib_Get_R8;


	--
	-- GET DATA FOR COLUMN AND CONVERT IT TO STRING TYPE
	--
	function CT_Lib_Convert(
		Cmd :		in	CT_Lib_Cmd_Type;
		Item :		in	Column_Index_Type;
		Src_Fmt :	in	Field_Type
	) return String_Ptr is
		function CT_Lib_To_String(Cmd : CT_Lib_Cmd_Type; Item : Int_Type; Fmt : Field_Type; Buf, Len : System.Address)
			return Int_Type;
		pragma import(C,CT_Lib_To_String,"c_ct_lib_to_string");

		Buf : String(1..512);
		Len : Natural := Buf'Length;
	begin

		case Src_Fmt is
			when Type_DATETIME | Type_DATETIME4 | Type_MONEY | Type_MONEY4 | Type_NUMERIC | Type_DECIMAL =>

				if CT_Lib_To_String(Cmd,Int_Type(Item),Src_Fmt,Buf(1)'Address,Len'Address) /= 0 then
					pragma assert(Len <= Buf'Length);
					if Len > 0 then
						return new String'(Buf(1..Len));
					else
						return null;
					end if;
				else
					raise Program_Error;
				end if;

			when Type_BIT =>

				if CT_Lib_To_String(Cmd,Int_Type(Item),Src_Fmt,Buf(1)'Address,Len'Address) /= 0 then
					pragma assert(Len <= Buf'Length);
					if Len > 0 then
						declare
							Text : String := Buf(1..Len);
						begin
							if Text = "1" then
								return new String'("TRUE");
							elsif Text = "0" then
								return new String'("FALSE");
							else
								raise Program_Error;
							end if;
						end;
					else
						return null;
					end if;
				else
					raise Program_Error;
				end if;

			when others =>
				raise Program_Error;
		end case;

	end CT_Lib_Convert;


	--
	-- Return String Data for column data
	--
	function CT_Lib_Get_Data(Cmd : CT_Lib_Cmd_Type; Item : Column_Index_Type; Described : CT_Lib_Column_Type) return String_Ptr is
		function Trim(S : String) return String_Ptr is
		begin
			if S(S'First) = ' ' then
				return new String'(S(S'Range));
			else
				return new String'(S);
			end if;
		end Trim;

		Is_Null : Boolean;
	begin

		case Described.Data_Type is

			when Type_CHAR | Type_LONGCHAR | Type_TEXT | Type_VARCHAR =>
				return CT_Lib_Get_Char_Data(Cmd,Item,Described.Max_Length);

			when Type_USHORT =>
				declare
					V : U2_Type;
				begin
					CT_Lib_Get_U2(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(U2_Type'Image(V));
					end if;
				end;

			when Type_TINYINT =>
				declare
					V : I1_Type;
				begin
					CT_Lib_Get_I1(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(I1_Type'Image(V));
					end if;
				end;

			when Type_SMALLINT =>
				declare
					V : I2_Type;
				begin
					CT_Lib_Get_I2(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(I2_Type'Image(V));
					end if;
				end;

			when Type_INT =>
				declare
					V : I4_Type;
				begin
					CT_Lib_Get_I4(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(I4_Type'Image(V));
					end if;
				end;

			when Type_LONG =>
				declare
					V : I8_Type;
				begin
					CT_Lib_Get_I8(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(I8_Type'Image(V));
					end if;
				end;

			when Type_FLOAT =>
				declare
					V : R8_Type;
				begin
					CT_Lib_Get_R8(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(R8_Type'Image(V));
					end if;
				end;

			when Type_REAL =>
				declare
					V : R4_Type;
				begin
					CT_Lib_Get_R4(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(R4_Type'Image(V));
					end if;
				end;

			when Type_VOID =>
				return null;		-- ???

			when Type_BIT =>
				return CT_Lib_Convert(Cmd,Item,Described.Data_Type);

			when Type_DATETIME | Type_DATETIME4  =>
				return CT_Lib_Convert(Cmd,Item,Described.Data_Type);

			when Type_MONEY | Type_MONEY4 | Type_NUMERIC | Type_DECIMAL =>
				return CT_Lib_Convert(Cmd,Item,Described.Data_Type);

			when Type_VARBINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY58: APQ does not support Type_VARBINARY (CT_Lib_Get_Data).");
				return null;
			when Type_BINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY59: APQ does not support Type_BINARY (CT_Lib_Get_Data).");
				return null;
			when Type_LONGBINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY60: APQ does not support Type_LONGBINARY (CT_Lib_Get_Data).");
				return null;
			when Type_IMAGE =>
				Raise_Exception(Not_Supported'Identity,
					"SY61: APQ does not support Type_IMAGE (CT_Lib_Get_Data).");
				return null;
			when Type_SENSITIVITY =>
				Raise_Exception(Not_Supported'Identity,
					"SY62: APQ does not support Type_SENSITIVITY (CT_Lib_Get_Data).");
				return null;
			when Type_BOUNDARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY63: APQ does not support Type_BOUNDARY (CT_Lib_Get_Data).");
				return null;
			when Type_UNICHAR =>
				Raise_Exception(Not_Supported'Identity,
					"SY64: APQ does not support Type_UNICHAR (CT_Lib_Get_Data).");
				return null;
		end case;

	end CT_Lib_Get_Data;


	--
	-- GET DATA FOR EACH COLUMN
	--
	procedure CT_Lib_Get_Data(Cmd : CT_Lib_Cmd_Type; Values : in out CT_Lib_Columns_Array) is
	begin

		for X in Values'Range loop
			Values(X).Str_Value := CT_Lib_Get_Data(Cmd,X,Values(X));
		end loop;

	end CT_Lib_Get_Data;



	procedure CT_Lib_Bool_Option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_ct_lib_bool_option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_ct_lib_bool_option,"c_ct_lib_bool_option");
	begin
		if c_ct_lib_bool_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY66: Failed to set a Sybase boolean option (CT_Lib_Bool_Option).");
		end if;
	end CT_Lib_Bool_Option;



	procedure CT_Lib_Uint_Option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.unsigned) is
		use Interfaces.C;

		function c_ct_lib_uint_option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.unsigned) return Interfaces.C.int;
		pragma import(C,c_ct_lib_uint_option,"c_ct_lib_uint_option");
	begin
		if c_ct_lib_uint_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY67: Failed to set a Sybase unsigned option (CT_Lib_Uint_Option).");
		end if;
	end CT_Lib_Uint_Option;



	procedure CT_Lib_DOW_Option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_ct_lib_dow_option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_ct_lib_dow_option,"c_ct_lib_dow_option");
	begin
		if c_ct_lib_dow_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY68: Failed to set a Sybase day-of-week option (CT_Lib_DOW_Option).");
		end if;
	end CT_Lib_DOW_Option;



	procedure CT_Lib_Format_Option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_ct_lib_format_option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_ct_lib_format_option,"c_ct_lib_format_option");
	begin
		if c_ct_lib_format_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY69: Failed to set a date format option (CT_Lib_Format_Option).");
		end if;
	end CT_Lib_Format_Option;



	procedure CT_Lib_String_Option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : System.Address) is
		use Interfaces.C;

		function c_ct_lib_string_option(Conn : CT_Lib_Conn_Type; E : CT_Lib_Enum_Option; Arg : System.Address) return Interfaces.C.int;
		pragma import(C,c_ct_lib_string_option,"c_ct_lib_string_option");
	begin
		if c_ct_lib_string_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY70: Failed to set a Sybase string option (CT_Lib_String_Option).");
		end if;
	end CT_Lib_String_Option;

	--
	-- FREE COLUMN DATA
	--
	procedure Free(Values : in out CT_Lib_Columns_Ptr; Release_Array : Boolean := True) is
		procedure Free_Array is new Ada.Unchecked_Deallocation(CT_Lib_Columns_Array,CT_Lib_Columns_Ptr);
	begin

		for X in Values.all'Range loop
			if Values.all(X).Str_Value /= null then
				Free(Values.all(X).Str_Value);
			end if;
		end loop;
		if Release_Array then
			Free_Array(Values);
		end if;

	end Free;


end APQ.CT_Lib;
