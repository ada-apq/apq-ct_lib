-- $Id: apq-sybase.adb,v 1.28 2004/10/05 00:48:30 wwg Exp $
-- Copyright (c) 2003, Warren W. Gay VE3WWG
--
-- Licensed under the ACL (Ada Community License)
-- or
-- GNU Public License 2 (GPL2)
-- 
--     This program is free software; you can redistribute it and/or modify
--     it under the terms of the GNU General Public License as published by
--     the Free Software Foundation; either version 2 of the License, or
--     (at your option) any later version.
-- 
--     This program is distributed in the hope that it will be useful,
--     but WITHOUT ANY WARRANTY; without even the implied warranty of
--     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--     GNU General Public License for more details.
-- 
--     You should have received a copy of the GNU General Public License
--     along with this program; if not, write to the Free Software
--     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

use Ada.Exceptions;

package body APQ.Sybase is

	No_Warnings : constant String(1..6) := "      ";

	-- 
	-- FINALIZATION FOR SQLCA_TYPE
	-- 
	procedure Finalize(SQLCA : in out SQLCA_Type) is
	begin
		if SQLCA.Sqlerrm /= null then
			Free(SQLCA.Sqlerrm);						-- Free error message string
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
	procedure Sy_Free_Context(Ctx : in out Sy_Context_Type) is
		function c_sy_free_context(Ctx : Sy_Context_Type) return Sy_Context_Type;
		pragma import(C,c_sy_free_context,"c_sy_free_context");
	begin
		Ctx := c_sy_free_context(Ctx);
	end Sy_Free_Context;


	--
	-- FREE SYBASE CONNECTION
	--
	procedure Sy_Free_Connection(Conn : in out Sy_Conn_Type) is
		function c_sy_free_connection(Conn : Sy_Conn_Type) return Sy_Conn_Type;
		pragma import(C,c_sy_free_connection,"c_sy_free_connection");
	begin
		Conn := c_sy_free_connection(Conn);
	end Sy_Free_Connection;


	--
	-- SET HOSTNAME TO USE FOR CONNECTION
	--
	function Sy_Set_Hostname(Conn : Sy_Conn_Type; Host : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_sy_set_hostname(Conn : Sy_Conn_Type; host : System.Address) return Return_Status;
		pragma import(C,c_sy_set_hostname,"c_sy_set_hostname");
		C_Host :	char_array_access;
		A_Host :	System.Address := System.Null_Address;
		RC :		Return_Status;
	begin
		C_String(To_String(Host),C_Host,A_Host);
		RC := c_sy_set_hostname(Conn,A_Host);
		if C_Host /= null then
			Free(C_Host);
		end if;
		return RC /= 0;
	end Sy_Set_Hostname;


	--
	-- SET SYBASE USERID
	--
	function Sy_Set_Userid(Conn : Sy_Conn_Type; Userid : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_sy_set_userid(Conn : Sy_Conn_Type; userid : System.Address) return Return_Status;
		pragma import(C,c_sy_set_userid,"c_sy_set_userid");
		C_Userid :	char_array_access;
		A_Userid :	System.Address;
		RC :			Return_Status;
	begin
		C_String(To_String(Userid),C_Userid,A_Userid);
		RC := c_sy_set_userid(Conn,A_Userid);
		if C_Userid /= null then
			Free(C_Userid);
		end if;
		return RC /= 0;
	end Sy_Set_Userid;


	--
	-- SET THE PASSWORD TO USE
	--
	function Sy_Set_Passwd(Conn : Sy_Conn_Type; Passwd : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_sy_set_passwd(Conn : Sy_Conn_Type; passwd : System.Address) return Return_Status;
		pragma import(C,c_sy_set_passwd,"c_sy_set_passwd");
		C_Passwd :	char_array_access;
		A_Passwd :	System.Address;
		RC :			Return_Status;
	begin
		C_String(To_String(Passwd),C_Passwd,A_Passwd);
		RC := c_sy_set_passwd(Conn,A_Passwd);
		if C_Passwd /= null then
			Free(C_Passwd);
		end if;
		return RC /= 0;
	end Sy_Set_Passwd;


	--
	-- SET THE DATABASE NAME TO USE
	--
	function Sy_Set_Database(Conn : Sy_Conn_Type; Database : String_Ptr) return Boolean is
		use Interfaces.C.Strings;
		function c_sy_set_database(Conn : Sy_Conn_Type; database : System.Address) return Return_Status;
		pragma import(C,c_sy_set_database,"c_sy_set_database");
		C_Db :	char_array_access;
		A_Db :	System.Address;
		RC :		Return_Status;
	begin
		C_String(To_String(Database),C_Db,A_Db);
		RC := c_sy_set_database(Conn,A_Db);
		if C_Db /= null then
			Free(C_Db);
		end if;
		return RC /= 0;
	end Sy_Set_Database;


	--
	-- CONNECT TO THE SYBASE SERVER
	--
	function Sy_Connect(Conn : Sy_Conn_Type; Instance : String) return Boolean is
		use Interfaces.C.Strings;
		function c_sy_connect(Conn : Sy_Conn_Type; instance : System.Address) return Return_Status;
		pragma import(C,c_sy_connect,"c_sy_connect");
		C_Instance :	char_array_access;
		A_Instance :	System.Address;
		RC :				Return_Status;
	begin
		C_String(Instance,C_Instance,A_Instance);
		RC := c_sy_connect(Conn,A_Instance);
		if C_Instance /= null then
			Free(C_Instance);
		end if;
		return RC /= 0;
	end Sy_Connect;


	--
	-- DISCONNECT FROM THE SYBASE SERVER
	--
	function Sy_Disconnect(Conn : Sy_Conn_Type) return Boolean is
		function c_sy_disconnect(Conn : Sy_Conn_Type) return Return_Status;
		pragma import(C,c_sy_disconnect,"c_sy_disconnect");
		RC : Return_Status;
	begin
		RC := c_sy_disconnect(Conn);
		return RC /= 0;
	end Sy_Disconnect;


	--
	-- ALLOCATE A SYBASE CS_COMMAND, SEND SQL TO SERVER, AND EXECUTE IT
	--
	function Sy_Exec(Conn : Sy_Conn_Type; SQL : String) return Sy_Cmd_Type is
		use Interfaces.C;
		function c_sy_exec(conn : Sy_Conn_Type; SQL : System.Address) return Sy_Cmd_Type;
		pragma import(C,c_sy_exec,"c_sy_exec");
		C_SQL : char_array := To_C(SQL);
	begin
		return c_sy_exec(Conn,C_SQL'Address);
	end Sy_Exec;


	--
	-- OPEN AN SQL CURSOR
	--
	function Sy_Open_Cursor(
		Conn :			Sy_Conn_Type;		-- Sybase connection
		SQL :				String;				-- SQL text
		Cursor_Name :	System.Address;	-- Start of cursor name
		Name_Length :	Natural;				-- Length of cursor name
		For_Update :	Boolean				-- True if cursor is for update
	) return Sy_Cmd_Type is
		use Interfaces.C;

		function c_sy_cursor(Conn : Sy_Conn_Type; Name : System.Address; Name_Len : Int_Type; SQL : System.Address; For_Update : Int_Type)
			return Sy_Cmd_Type;
		pragma import(C,c_sy_cursor,"c_sy_cursor");

		C_SQL :	char_array := To_C(SQL);
		Update :	Int_Type := 0;
	begin
		if For_Update then
			Update := 1;
		end if;
		return c_sy_cursor(Conn,Cursor_Name,Int_Type(Name_Length),C_SQL'Address,Update);
	end Sy_Open_Cursor;


	--
	-- CLOSE & DEALLOCATE SQL CURSOR (BUT DO NOT DEALLOCATE SY_CMD_TYPE)
	--
	function Sy_Close_Cursor(Cmd : Sy_Cmd_Type) return Boolean is
		function c_sy_close(Conn : Sy_Cmd_Type) return Int_Type;
		pragma import(C,c_sy_close,"c_sy_close");
	begin
		return c_sy_close(Cmd) /= 0;
	end Sy_Close_Cursor;


	--
	-- SYBASE CT_RESULTS() CALL :
	--
	procedure Sy_Results(Cmd : Sy_Cmd_Type; Results : out Result_Type; Columns : out Natural) is
		function c_sy_results(Cmd : Sy_Cmd_Type; Cols : System.Address) return Result_Type;
		pragma import(C,c_sy_results,"c_sy_results");

		Num_Cols : Int_Type := 0;
	begin
		Results := c_sy_results(Cmd,Num_Cols'Address);
		Columns := Natural(Num_Cols);
	end Sy_Results;


	--
	-- Special Result check for CS_CMD_DONE
	--
	function Sy_Is_Done(Cmd : Sy_Cmd_Type) return Boolean is
		function c_sy_isdone(Cmd : Sy_Cmd_Type) return Int_Type;
		pragma import(C,c_sy_isdone,"c_sy_isdone");
	begin
		return c_sy_isdone(Cmd) /= 0;
	end Sy_Is_Done;


	--
	-- Special Result check for CS_CMD_DONE & CS_END_RESULTS
	--
	function Sy_Is_End(Cmd : Sy_Cmd_Type) return Boolean is
		function c_sy_isend(Cmd : Sy_Cmd_Type) return Int_Type;
		pragma import(C,c_sy_isend,"c_sy_isend");
	begin
		return c_sy_isend(Cmd) /= 0;
	end Sy_Is_End;


	--
	-- Cancel pending results
	--
	function Sy_Cancel(Cmd : Sy_Cmd_Type) return Sy_Cancel_Type is
		function c_sy_cancel(Cmd : Sy_Cmd_Type) return Int_Type;
		pragma import(C,c_sy_cancel,"c_sy_cancel");

		R : Int_Type;
	begin
		R := c_sy_cancel(Cmd);
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
	end Sy_Cancel;


	--
	-- Print a debug trace of Sybase results to stderr
	--
	procedure Sy_Debug_Results(Cmd : Sy_Cmd_Type) is
		procedure c_sy_dbg_results(Cmd : Sy_Cmd_Type);
		pragma import(C,c_sy_dbg_results,"c_sy_dbg_results");
	begin
		c_sy_dbg_results(Cmd);
	end Sy_Debug_Results;


	--
	-- DESCRIBE THE COLUMNS FROM A RESULT :
	--
	function Sy_Describe(Cmd : Sy_Cmd_Type; Columns : Positive) return Sy_Columns_Ptr is
		procedure c_sy_describe(
			Cmd :				in		Sy_Cmd_Type;
			Item :			in		Int_Type;
			Name :			in		System.Address;
			Name_Size :		in		System.Address;
			Data_Type :		in		System.Address;
			Status :			in		System.Address;
			Max_Length :	in		System.Address;
			Scale :			in		System.Address;
			Precision :		in		System.Address
		);
		pragma import(C,c_sy_describe,"c_sy_describe");

		D : Sy_Columns_Ptr := new Sy_Columns_Array(1..Column_Index_Type(Columns));
	begin

		for X in D'Range loop
			D(X).Name_Length := D(X).Name'Length;
			c_sy_describe(
				Cmd			=> Cmd,
				Item			=> Int_Type(X),
				Name			=> D(X).Name(1)'Address,
				Name_Size	=> D(X).Name_Length'Address,
				Data_Type	=> D(X).Data_Type'Address,
				Status		=> D(X).Status'Address,
				Max_Length	=> D(X).Max_Length'Address,
				Scale			=> D(X).Scale'Address,
				Precision	=> D(X).Precision'Address
			);
			D(X).Str_Value := null;
		end loop;
		return D;

	end Sy_Describe;


	--
	-- SYBASE CT_GET_DATA() :
	--
	procedure Sy_Get_Data(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; Buf : System.Address; Len : Natural; Out_Len : out Natural; RC : out Get_Data_Type) is
		function c_sy_get_data(Cmd : Sy_Cmd_Type; Item : Int_Type; Buf : System.Address; Len : Int_Type; Out_Len : System.Address) return Get_Data_Type;
		pragma import(C,c_sy_get_data,"c_sy_get_data");
		Rtn : Int_Type;
	begin
		RC := c_sy_get_data(Cmd,Int_Type(Item),Buf,Int_Type(Len),Rtn'Address);
		Out_Len := Natural(Rtn);
	end Sy_Get_Data;


	--
	-- GET CHAR_TYPE DATA :
	--
	function Sy_Get_Char_Data(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; Max_Length : Natural ) return String_Ptr is
		use Ada.Strings.Unbounded;

		Ret_Code :	Get_Data_Type;
		Char_Buf :	String(1..Max_Length);
		Char_Len :	Natural;
		Char_Val :	Unbounded_String;
	begin

		loop
			Sy_Get_Data(Cmd,Item,Char_Buf(1)'Address,Char_Buf'Length,Char_Len,Ret_Code);
			exit when Ret_Code = Get_Data_Failed;
			if Char_Len > 0 then
				Append(Char_Val,Char_Buf(1..Char_Len));
			end if;
			exit when Ret_Code = Last_Chunk;
		end loop;

		if Ret_Code /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY50: Unable to fetch all character data (Sy_Get_Char_Data).");
		end if;

		if Length(Char_Val) < 1 then
			return null;						-- The value is null
		else
			return new String'(To_String(Char_Val));
		end if;

	end Sy_Get_Char_Data;


	--
	-- GET 2 BYTE INTEGER VALUE
	--
	procedure Sy_Get_U2(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out U2_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		U2 :	U2_Type;
	begin
		Sy_Get_Data(Cmd,Item,U2'Address,2,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY51: Unable to get all data (Sy_Get_U2).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 2);
			V := U2;
		else
			V := 0;
		end if;
	end Sy_Get_U2;


	--
	-- GET 1 BYTE INTEGER VALUE
	--
	procedure Sy_Get_I1(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out I1_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I1 :	I1_Type;
	begin
		Sy_Get_Data(Cmd,Item,I1'Address,1,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY52: Unable to get all data (Sy_Get_I1).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 1);
			V := I1;
		else
			V := 0;
		end if;
	end Sy_Get_I1;


	--
	-- GET 2 BYTE INTEGER VALUE
	--
	procedure Sy_Get_I2(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out I2_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I2 :	I2_Type;
	begin
		Sy_Get_Data(Cmd,Item,I2'Address,2,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY53: Unable to get all data (Sy_Get_I2).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 2);
			V := I2;
		else
			V := 0;
		end if;
	end Sy_Get_I2;


	--
	-- GET 4 BYTE INTEGER VALUE
	--
	procedure Sy_Get_I4(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out I4_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I4 :	I4_Type;
	begin
		Sy_Get_Data(Cmd,Item,I4'Address,4,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY54: Unable to get all data (Sy_Get_I4).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 4);
			V := I4;
		else
			V := 0;
		end if;
	end Sy_Get_I4;


	--
	-- GET 8 BYTE INTEGER VALUE
	--
	procedure Sy_Get_I8(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out I8_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		I8 :	I8_Type;
	begin
		Sy_Get_Data(Cmd,Item,I8'Address,8,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY55: Unable to get all data (Sy_Get_I8).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 8);
			V := I8;
		else
			V := 0;
		end if;
	end Sy_Get_I8;


	--
	-- GET 4 BYTE REAL VALUE
	--
	procedure Sy_Get_R4(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out R4_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		R4 :	R4_Type;
	begin
		Sy_Get_Data(Cmd,Item,R4'Address,4,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY56: Unable to get all data (Sy_Get_R4).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 4);
			V := R4;
		else
			V := 0.0;
		end if;
	end Sy_Get_R4;


	--
	-- GET 8 BYTE REAL VALUE
	--
	procedure Sy_Get_R8(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; V : out R8_Type; Is_Null : out Boolean) is
		RC :	Get_Data_Type;
		Len :	Natural;
		R8 :	R8_Type;
	begin
		Sy_Get_Data(Cmd,Item,R8'Address,8,Len,RC);
		if RC /= Last_Chunk then
			Raise_Exception(Failed'Identity,
				"SY57: Unable to get all data (Sy_Get_R8).");
		end if;
		Is_Null := Len = 0;
		if not Is_Null then
			pragma assert(Len = 8);
			V := R8;
		else
			V := 0.0;
		end if;
	end Sy_Get_R8;


	--
	-- GET DATA FOR COLUMN AND CONVERT IT TO STRING TYPE
	--
	function Sy_Convert(
		Cmd :			in		Sy_Cmd_Type;
		Item :		in		Column_Index_Type;
		Src_Fmt :	in		Field_Type
	) return String_Ptr is
		function Sy_To_String(Cmd : Sy_Cmd_Type; Item : Int_Type; Fmt : Field_Type; Buf, Len : System.Address)
			return Int_Type;
		pragma import(C,Sy_To_String,"c_sy_to_string");

		Buf : String(1..512);
		Len : Natural := Buf'Length;
	begin

		case Src_Fmt is
			when Type_DATETIME | Type_DATETIME4 | Type_MONEY | Type_MONEY4 | Type_NUMERIC | Type_DECIMAL =>

				if Sy_To_String(Cmd,Int_Type(Item),Src_Fmt,Buf(1)'Address,Len'Address) /= 0 then
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

				if Sy_To_String(Cmd,Int_Type(Item),Src_Fmt,Buf(1)'Address,Len'Address) /= 0 then
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

	end Sy_Convert;


	--
	-- Return String Data for column data
	--
	function Sy_Get_Data(Cmd : Sy_Cmd_Type; Item : Column_Index_Type; Described : Sy_Column_Type) return String_Ptr is
		function Trim(S : String) return String_Ptr is
		begin
			if S(S'First) = ' ' then
				return new String'(S(S'First+1..S'Length));
			else
				return new String'(S);
			end if;
		end Trim;

		Is_Null : Boolean;
	begin

		case Described.Data_Type is

			when Type_CHAR | Type_LONGCHAR | Type_TEXT | Type_VARCHAR =>
				return Sy_Get_Char_Data(Cmd,Item,Described.Max_Length);

			when Type_USHORT =>
				declare
					V : U2_Type;
				begin
					Sy_Get_U2(Cmd,Item,V,Is_Null);
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
					Sy_Get_I1(Cmd,Item,V,Is_Null);
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
					Sy_Get_I2(Cmd,Item,V,Is_Null);
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
					Sy_Get_I4(Cmd,Item,V,Is_Null);
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
					Sy_Get_I8(Cmd,Item,V,Is_Null);
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
					Sy_Get_R8(Cmd,Item,V,Is_Null);
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
					Sy_Get_R4(Cmd,Item,V,Is_Null);
					if Is_Null then
						return null;
					else
						return Trim(R4_Type'Image(V));
					end if;
				end;

			when Type_VOID =>
				return null;			-- ???

			when Type_BIT =>
				return Sy_Convert(Cmd,Item,Described.Data_Type);

			when Type_DATETIME | Type_DATETIME4  =>
				return Sy_Convert(Cmd,Item,Described.Data_Type);

			when Type_MONEY | Type_MONEY4 | Type_NUMERIC | Type_DECIMAL =>
				return Sy_Convert(Cmd,Item,Described.Data_Type);

			when Type_VARBINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY58: APQ does not support Type_VARBINARY (Sy_Get_Data).");
				return null;
			when Type_BINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY59: APQ does not support Type_BINARY (Sy_Get_Data).");
				return null;
			when Type_LONGBINARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY60: APQ does not support Type_LONGBINARY (Sy_Get_Data).");
				return null;
			when Type_IMAGE =>
				Raise_Exception(Not_Supported'Identity,
					"SY61: APQ does not support Type_IMAGE (Sy_Get_Data).");
				return null;
			when Type_SENSITIVITY =>
				Raise_Exception(Not_Supported'Identity,
					"SY62: APQ does not support Type_SENSITIVITY (Sy_Get_Data).");
				return null;
			when Type_BOUNDARY =>
				Raise_Exception(Not_Supported'Identity,
					"SY63: APQ does not support Type_BOUNDARY (Sy_Get_Data).");
				return null;
			when Type_UNICHAR =>
				Raise_Exception(Not_Supported'Identity,
					"SY64: APQ does not support Type_UNICHAR (Sy_Get_Data).");
				return null;
		end case;

	end Sy_Get_Data;


	--
	-- GET DATA FOR EACH COLUMN
	--
	procedure Sy_Get_Data(Cmd : Sy_Cmd_Type; Values : in out Sy_Columns_Array) is
	begin

		for X in Values'Range loop
			Values(X).Str_Value := Sy_Get_Data(Cmd,X,Values(X));
		end loop;

	end Sy_Get_Data;


	--
	-- FREE COLUMN DATA
	--
	procedure Free(Values : in out Sy_Columns_Ptr; Release_Array : Boolean := True) is
		procedure Free_Array is new Ada.Unchecked_Deallocation(Sy_Columns_Array,Sy_Columns_Ptr);
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



	procedure Sy_Bool_Option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_sy_bool_option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_sy_bool_option,"c_sy_bool_option");
	begin
		if c_sy_bool_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY66: Failed to set a Sybase boolean option (Sy_Bool_Option).");
		end if;
	end Sy_Bool_Option;



	procedure Sy_Uint_Option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.unsigned) is
		use Interfaces.C;

		function c_sy_uint_option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.unsigned) return Interfaces.C.int;
		pragma import(C,c_sy_uint_option,"c_sy_uint_option");
	begin
		if c_sy_uint_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY67: Failed to set a Sybase unsigned option (Sy_Uint_Option).");
		end if;
	end Sy_Uint_Option;



	procedure Sy_DOW_Option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_sy_dow_option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_sy_dow_option,"c_sy_dow_option");
	begin
		if c_sy_dow_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY68: Failed to set a Sybase day-of-week option (Sy_DOW_Option).");
		end if;
	end Sy_DOW_Option;



	procedure Sy_Format_Option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) is
		use Interfaces.C;

		function c_sy_format_option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : Interfaces.C.int) return Interfaces.C.int;
		pragma import(C,c_sy_format_option,"c_sy_format_option");
	begin
		if c_sy_format_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY69: Failed to set a date format option (Sy_Format_Option).");
		end if;
	end Sy_Format_Option;



	procedure Sy_String_Option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : System.Address) is
		use Interfaces.C;

		function c_sy_string_option(Conn : Sy_Conn_Type; E : Sybase_Enum_Option; Arg : System.Address) return Interfaces.C.int;
		pragma import(C,c_sy_string_option,"c_sy_string_option");
	begin
		if c_sy_string_option(Conn,E,Arg) = 0 then
			Raise_Exception(Failed'Identity,
				"SY70: Failed to set a Sybase string option (Sy_String_Option).");
		end if;
	end Sy_String_Option;

end APQ.Sybase;
