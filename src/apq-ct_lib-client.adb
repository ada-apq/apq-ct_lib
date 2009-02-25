------------------------------------------------------------------------------
--                                                                          --
--                          APQ DATABASE BINDINGS                           --
--                                                                          --
--                              A P Q - CT_Lib 				    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Warren W. Gay VE3WWG                    --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
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

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.IO_Exceptions;
with System.Address_To_Access_Conversions;
with Interfaces.C.Strings;

use Ada.Exceptions;


package body APQ.CT_Lib.Client is

	type Field_Count_Type is mod 2 ** 32;

	package Conn_Addr is new System.Address_To_Access_Conversions(Connection_Type);

	procedure Internal_Reset0(C : in out Connection_Type; In_Finalize : Boolean := False);
	procedure Internal_Reset1(C : in out Connection_Type; In_Finalize : Boolean := False);


	protected Name_Generator is
		procedure Generate(Q : Query_Type; Cursor_Name : out Cursor_Name_Type);
	private
		Cursor_No : Natural := 0;		-- Last referenced Cursor #
	end Name_Generator;


	protected body Name_Generator is

		procedure Generate(Q : Query_Type; Cursor_Name : out Cursor_Name_Type) is
		begin

			Cursor_No := Cursor_No + 1;

			declare
				G : String(1..Cursor_Name'Length) := ( others => '0' );
				S : String := Natural'Image(Cursor_No);
				X : Natural := Cursor_Name'Length - S'Length;
			begin
				G(1+X+1..Cursor_Name'Last) := S(S'First+1..S'Last);
				G(1) := 'C';
				Cursor_Name := Cursor_Name_Type(To_Case(String(Cursor_Name_Type(G)),Q.SQL_Case));
			end;

		end Generate;

	end Name_Generator;



	procedure Report_Error(File : Ada.Text_IO.File_Type; SQLCA : SQLCA_Type) is
		use Ada.Text_IO;
	begin

		Put_Line(File,"-- SQLCA :  Sqlcode: " & SQL_Code_Type'Image(SQLCA.Sqlcode));
		if SQLCA.Sqlerrm /= null then
			Put_Line(File,"--          Sqlerrm: " & SQLCA.Sqlerrm.all);
		else
			Put_Line(File,"--          Sqlerrm: ");
		end if;
		Put_Line(File,"--          Sqlerrp: " & SQLCA.Sqlerrp);
		Put_Line(File,"--    Rows Affected: " & Long_Type'Image(SQLCA.Rows_Affected));
		Put_Line(File,"--          Sqlwarn: '" & SQLCA.Sqlwarn & "'");
		New_Line(File);

	end Report_Error;



	------------------------------
	-- DATABASE CONNECTION :
	------------------------------


	-- INTERNAL


	procedure Clear_Error(C : in out Connection_Type) is
	begin
		Clear_SQLCA(C.SQLCA.all);
	end Clear_Error;


	--
	-- Tell CT_Lib to use another database
	--
	procedure Use_Database(C : in out Connection_Type; DB_Name : String) is
		Q : Query_Type;
		USE_SQL : String := "USE " & To_Case(DB_Name,C.SQL_Case);
		Date_Format_SQL : String := "set DATEFORMAT ymd";
	begin
		begin

			-- USE --
			Prepare( Q, Use_SQL );
			Execute( Q, C );

			-- set DATEFORMAT
			Prepare( Q, Date_Format_SQL );
			Execute( Q, C );

		exception
			when SQL_Error =>
				Raise_Exception(APQ.Use_Error'Identity,
					"SY01: Unable to select database '" & DB_Name & "' (Use_Database).");
		end;
	end Use_Database;



	procedure Parse_Option(
		Options :	in out	Ada.Strings.Unbounded.Unbounded_String;
		Keyword :	in out	Ada.Strings.Unbounded.Unbounded_String;
		Argument :	in out	Ada.Strings.Unbounded.Unbounded_String
	) is
		use Ada.Strings.Unbounded;
		Option :  Unbounded_String;
		The_End : Natural;
		Value_X : Natural;
	begin
		Keyword := Null_Unbounded_String;
		Argument := Null_Unbounded_String;

		while Length(Options) > 0 loop
			exit when Slice(Options,1,1) /= ",";
			Delete(Options,1,1);
		end loop;

		if Length(Options) < 1 then
			return;
		end if;

		The_End := Index(Options,",");
		if The_End < 1 then
			The_End := Length(Options)+1;
		end if;

		Option := Options;
		if The_End <= Length(Options) then
			Delete(Option,The_End,Length(Options));
		end if;

		if The_End <= Length(Options) then
			Delete(Options,1,The_End);
		else
			Delete(Options,1,The_End-1);
		end if;

		Value_X := Index(Option,"=");
		if Value_X < 1 then
			Keyword := Option;
		else
			Keyword := To_Unbounded_String(Slice(Option,1,Value_X-1));
			Argument := To_Unbounded_String(Slice(Option,Value_X+1,Length(Option)));
		end if;

	end Parse_Option;



	procedure Process_Option(C : in out Connection_Type; Keyword, Argument : String) is
		use Interfaces.C, Ada.Characters.Handling;

		L : Natural;
		X : Natural := 0;
		E : CT_Lib_Enum_Option;
		T : Option_Argument_Type;
	begin

		for Y in APQ.CT_Lib.Options'Range loop
			L := APQ.CT_Lib.Options(Y).Length;
			if APQ.CT_Lib.Options(Y).Name(1..L) = Keyword then
				X := Y;
				exit;
			end if;
		end loop;

		if X = 0 then
			Raise_Exception(Failed'Identity,
				"SY02: Unknown option '" & Keyword & "' (CT_Lib).");
		end if;

		E := APQ.CT_Lib.Options(X).CT_Lib_Enum;		-- Database option value
		T := APQ.CT_Lib.Options(X).Argument;		-- Argument type

		case T is
			when ARG_BOOLEAN =>
				declare
					Arg : String := To_Upper(Argument);
					TF : int := 0;
				begin
					if Arg = "T" or else Arg = "TRUE" then
						TF := 1;
					elsif Arg = "F" or else Arg = "FALSE" then
						TF := 0;
					else
						Raise_Exception(Failed'Identity,
							"SY03: Argument for option " & Keyword & " must be Boolean.");
					end if;
					CT_Lib_Bool_Option(C.Connection,E,TF);
				end;
			when ARG_UINT =>
				declare
					U : unsigned;
				begin
					U := unsigned'Value(Argument);
					CT_Lib_Uint_Option(C.Connection,E,U);
				end;
			when ARG_DAY_OF_WEEK =>
				declare
					Arg : String := To_Upper(Argument);
					DOW : int := 0;
				begin
					if Arg = "SUNDAY" then
						DOW := 0;
					elsif Arg = "MONDAY" then
						DOW := 1;
					elsif Arg = "TUESDAY" then
						DOW := 2;
					elsif Arg = "WEDNESDAY" then
						DOW := 3;
					elsif Arg = "THURSDAY" then
						DOW := 4;
					elsif Arg = "FRIDAY" then
						DOW := 5;
					elsif Arg = "SATURDAY" then
						DOW := 6;
					else
						Raise_Exception(Failed'Identity,
							"SY04: Option argument for " & Keyword & " must be a weekday (SUNDAY..SATURDAY).");
					end if;
					CT_Lib_DOW_Option(C.Connection,E,DOW);
				end;
			when ARG_DATEFORMAT =>
				declare
					Arg : String := To_Upper(Argument);
					Fmt : int := 0;
				begin
					if Arg = "MDY" then
						Fmt := 0;
					elsif Arg = "DMY" then
						Fmt := 1;
					elsif Arg = "YMD" then
						Fmt := 2;
					elsif Arg = "YDM" then
						Fmt := 3;
					elsif Arg = "MYD" then
						Fmt := 4;
					elsif Arg = "DYM" then
						Fmt := 5;
					else
						Raise_Exception(Failed'Identity,
							"SY05: Option argument for " & Keyword & " must be of the form MDY/DMY/YMD etc.");
					end if;
					CT_Lib_Format_Option(C.Connection,E,Fmt);
				end;
			when ARG_CHAR_PTR =>
				declare
					S : char_array := To_C(Argument);
				begin
					CT_Lib_String_Option(C.Connection,E,S'Address);
				end;
			when ARG_CHAR_PTR_NULL =>
				declare
					UC :	String := To_Upper(Argument);
					S :	char_array := To_C(Argument);
				begin
					if UC = "NULL" then
						CT_Lib_String_Option(C.Connection,E,System.Null_Address);
					else
						CT_Lib_String_Option(C.Connection,E,S'Address);
					end if;
				end;
		end case;

	end Process_Option;



	procedure Process_Connection_Options(C : in out Connection_Type) is
		use Ada.Strings.Unbounded, Ada.Characters.Handling;
		Opts :		Unbounded_String;
		Keyword :	Unbounded_String;
		Argument :	Unbounded_String;
	begin
		Opts := To_Unbounded_String(C.Options.all);

		while Length(Opts) > 0 loop
			Parse_Option(Opts,Keyword,Argument);
			Process_Option(C,To_Upper(To_String(Keyword)),To_String(Argument));
		end loop;
	end Process_Connection_Options;



	procedure Internal_Reset0(C : in out Connection_Type; In_Finalize : Boolean := False) is
		Do_Commit : Boolean := False;
	begin

		Free(C.CT_Lib_Database);		-- Free this string, if any
		Clear_Error(C);			-- Clear errors

		if C.Connection /= Null_Connection then
			-- Abort query, and rollback..
			declare
				Q : Query_Type;
			begin
				Clear_Abort_State(C);
				if C.Rollback_Finalize or In_Abort_State(C) then
					if C.Trace_On and then C.Trace_Filename /= null and then In_Finalize = True then
						Ada.Text_IO.Put_Line(C.Trace_Ada,"-- ROLLBACK ON FINALIZE (FOLLOWS)");
					end if;
					Rollback_Work(Q,C);
				else
					if C.Trace_On and then C.Trace_Filename /= null and then In_Finalize = True then
						Ada.Text_IO.Put_Line(C.Trace_Ada,"-- COMMIT ON FINALIZE (FOLLOWS)");
					end if;
					Commit_Work(Q,C);
				end if;
			exception
				when others =>
					if C.Trace_On and then C.Trace_Filename /= null and then In_Finalize = True then
						Ada.Text_IO.Put_Line(C.Trace_Ada,"-- NO COMMIT/ROLLBACK PERFORMED");
					end if;
			end;

			Clear_Abort_State(C);
			Disconnect(C);

			if C.Trace_Filename /= null then
				Close_DB_Trace(C);
			end if;
		end if;
	end Internal_Reset0;



	procedure Internal_Reset1(C : in out Connection_Type; In_Finalize : Boolean := False) is
	begin
		if In_Finalize then
			Free_Ptr(C.Instance);
			Free_Ptr(C.Host_Name);
			Free_Ptr(C.Host_Address);
			Free_Ptr(C.DB_Name);
			Free_Ptr(C.User_Name);
			Free_Ptr(C.User_Password);
		end if;
	end Internal_Reset1;



	procedure Internal_Reset(C : in out Connection_Type; In_Finalize : Boolean := False) is
	begin
		Internal_Reset0(C,In_Finalize);
		Internal_Reset1(C,In_Finalize);
	end Internal_Reset;



	-- END INTERNAL



	function Engine_Of(C : Connection_Type) return Database_Type is
	begin
		return Engine_CT_Lib;
	end Engine_Of;



	function New_Query(C : Connection_Type) return Root_Query_Type'Class is
		Q : Query_Type;
	begin
		return Q;
	end New_Query;


	--
	-- Set the Database Name to be used:
	--
	--	 For CT_Lib, this is ignored at connection time. We make
	--	 note of the application wishes if not connected, until a
	--	 query is ready to be executed. Then the database is changed.
	--	 
	--	 However, if this routine is called while connected, we'll
	--	 make the database change immediately, so that it is easier
	--	 to test for a failed change (caller can catch Use_Error).
	--
	procedure Set_DB_Name(C : in out Connection_Type; DB_Name : String) is
	begin
		if not Is_Connected(C) then
			C.CT_Lib_Database := new String'(To_Case(DB_Name,C.SQL_Case));
		else
			Use_Database(C,DB_Name);	-- Use the specified database
			Set_DB_Name(Root_Connection_Type(C),To_Case(DB_Name,C.SQL_Case));
		end if;
	end Set_DB_Name;



	procedure Set_Options(C : in out Connection_Type; Options : String) is
		use Ada.Strings.Unbounded;
	begin
		Replace_String(C.Options,Set_Options.Options);

		if C.Options = null then
			return;
		end if;

		if Is_Connected(C) then
			Process_Connection_Options(C);
		end if;

	end Set_Options;



	function Options(C : Connection_Type) return String is
	begin
		return To_String(C.Options);
	end Options;



	procedure Connect(C : in out Connection_Type; Check_Connection : Boolean := True) is
		use Interfaces.C.Strings;
		A_Unix :	System.Address := System.Null_Address;
	begin

		Clear_Error(C);

		if Check_Connection and then Is_Connected(C) then
			Raise_Exception(Already_Connected'Identity,
				"SY06: Already connected (Connect).");
		end if;

		if  CT_Lib_Set_Hostname(C.Connection,C.Host_Name)		-- Configure host name
		and CT_Lib_Set_Userid(C.Connection,C.User_Name)		-- Configure userid
		and CT_Lib_Set_Passwd(C.Connection,C.User_Password)		-- Configure password
		and CT_Lib_Set_Database(C.Connection,C.DB_Name) then	-- Configure database(application) name
			if C.Instance /= null then
				C.Connected := CT_Lib_Connect(C.Connection,C.Instance.all);	-- Connect
			else
				C.Connected := CT_Lib_Connect(C.Connection,"");
			end if;
		end if;

		if C.Connected then
			--
			-- Process any deposited options
			--
			if C.Options /= null then
				Process_Connection_Options(C);
			end if;

			--
			-- See if we have a pending database change
			--
			if C.CT_Lib_Database /= null then
				declare
					Use_Raised : Boolean := False;
					Use_DB_Name : String := To_Case(C.CT_Lib_Database.all,C.SQL_Case);
				begin
					begin
						Free(C.CT_Lib_Database);
						Use_Database(C,Use_DB_Name);
					exception
						when APQ.Use_Error =>
							Use_Raised := True;
					end;
					if Use_Raised then
						Internal_Reset0(C);		-- Clear connection, but keep parameters
						Raise_Exception(APQ.Use_Error'Identity,
							"SY07: Unable to select database '" & Use_DB_Name & "' (Connect).");
					end if;
				end;
			end if;
		else
			Raise_Exception(Not_Connected'Identity,
				"SY08: Failed to connect to the database server (Connect).");
		end if;

	end Connect;



	procedure Connect(C : in out Connection_Type; Same_As : Root_Connection_Type'Class) is
		type Info_Func is access function(C : Connection_Type) return String;

		procedure Clone(S : in out String_Ptr; Get_Info : Info_Func) is
			Info : String := Get_Info(Connection_Type(Same_As));
		begin
			if Info'Length > 0 then
				S	:= new String(1..Info'Length);
				S.all	:= Info;
			else
				null;
				pragma assert(S = null);
			end if;
		end Clone;
	begin
		Reset(C);

		Clone(C.Host_Name,Host_Name'Access);

		C.Port_Format := Same_As.Port_Format;
		if C.Port_Format = IP_Port then
			C.Port_Number := Port(Same_As);		-- IP_Port
		else
			Clone(C.Port_Name,Port'Access);		-- UNIX_Port
		end if;

		Clone(C.DB_Name,DB_Name'Access);
		Clone(C.User_Name,User'Access);
		Clone(C.User_Password,Password'Access);
		Clone(C.Options,Options'Access);

		C.Rollback_Finalize  := Same_As.Rollback_Finalize;

		Connect(C);		-- Connect to database before worrying about trace facilities

		-- TRACE FILE & TRACE SETTINGS ARE NOT CLONED

	end Connect;



	procedure Disconnect(C : in out Connection_Type) is
	begin
		if C.Connection /= Null_Connection then
			if C.Connected then
				if not CT_Lib_Disconnect(C.Connection) then
					C.Connection := Null_Connection;
					Raise_Exception(Not_Connected'Identity,
						"SY09: Not connected (Disconnect).");
				end if;
			end if;
		end if;
		C.Connected  := False;
		C.Connection := Null_Connection;
	end Disconnect;



	function Is_Connected(C : Connection_Type) return Boolean is


	begin
		if C.Connection /= Null_Connection and then C.Connected then
			return Ct_Lib_Is_Connected( C.Connection ) = 1;
		else
			return False;
		end if;
	end Is_Connected;



	procedure Reset(C : in out Connection_Type) is
	begin
		Internal_Reset(C,False);
	end Reset;



	function Error_Message(C : Connection_Type) return String is
	begin
		if C.SQLCA.Sqlerrm /= null then
			return C.SQLCA.Sqlerrm.all;
		else
			return "";
		end if;
	end Error_Message;



	procedure Open_DB_Trace(C : in out Connection_Type; Filename : String; Mode : Trace_Mode_Type := Trace_APQ) is
	begin
		if C.Trace_Filename /= null then
			Raise_Exception(Tracing_State'Identity,
				"SY35: Already in trace mode (Open_DB_Trace).");
		end if;

		if not Is_Connected(C) then
			Raise_Exception(Not_Connected'Identity,
				"SY36: Unconected object (Open_DB_Trace).");
		end if;

		if Mode = Trace_None then
			pragma assert(C.Trace_Mode = Trace_None);
			return;		-- No trace required
		end if;

		Ada.Text_IO.Create(C.Trace_Ada,Append_File,Filename,Form => "shared=yes");
		C.Trace_File := CStr.Null_Stream;		-- Not used for CT_Lib

		Ada.Text_IO.Put_Line(C.Trace_Ada,"-- Start of Trace, Mode = " & Trace_Mode_Type'Image(Mode));

		C.Trace_Filename	:= new String(1..Filename'Length);
		C.Trace_Filename.all	:= Filename;
		C.Trace_Mode		:= Mode;
		C.Trace_On		:= True;		-- Enabled by default until Set_Trace disables this

	end Open_DB_Trace;



	procedure Close_DB_Trace(C : in out Connection_Type) is
	begin

		if C.Trace_Mode = Trace_None then
			return;		-- No tracing in progress
		end if;

		pragma assert(C.Trace_Filename /= null);

		Free(C.Trace_Filename);

		Ada.Text_IO.Put_Line(C.Trace_Ada,"-- End of Trace.");
		Ada.Text_IO.Close(C.Trace_Ada);	-- C.Trace_File is not used for APQ.CT_Lib

		C.Trace_Mode	:= Trace_None;
		C.Trace_On	:= True;		-- Restore default

	end Close_DB_Trace;



	procedure Set_Trace(C : in out Connection_Type; Trace_On : Boolean := True) is
		Orig_Trace : Boolean := C.Trace_On;
	begin
		C.Trace_On := Set_Trace.Trace_On;

		if Orig_Trace = C.Trace_On then
			return;		-- No change
		end if;

		if C.Trace_On then
			if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
				null;
			end if;
		else
			if C.Trace_Mode = Trace_DB or C.Trace_Mode = Trace_Full then
				null;
			end if;
		end if;
	end Set_Trace;



	function Is_Trace(C : Connection_Type) return Boolean is
	begin
		return C.Trace_On;
	end Is_Trace;



	function In_Abort_State(C : Connection_Type) return Boolean is
	begin
		if C.Connection = Null_Connection then
			return False;
		end if;
		return C.Abort_State;
	end In_Abort_State;




	------------------------------
	-- SQL QUERY API :
	------------------------------


	-- INTERNAL


	procedure Clear_Error(Q : in out Query_Type; C : in out Connection_Type) is
	begin
		Clear_Error(C);
		Q.SQLCA := C.SQLCA;		-- Share same SQLCA
	end Clear_Error;



	procedure Clear_Results(Q : in out Query_Type) is
	begin

		if Q.Values /= null then
			Free(Q.Values);
		end if;

		if Q.Cmd /= Null_Command then
			case Q.Results is
				when Execution_Failed =>
					null;
				when No_Results =>
					null;
				when Row_Results =>
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							raise Program_Error;
					end case;
				when Cursor_Results =>
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							if not CT_Lib_Close_Cursor(Q.Cmd) then
								raise Program_Error;		-- Cursor failed to close
							end if;
					end case;
				when Info_Results =>
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							raise Program_Error;
					end case;
				when Compute_Results =>			
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							raise Program_Error;
					end case;
				when Param_Results =>
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							raise Program_Error;
					end case;
				when Status_Results =>
					case CT_Lib_Cancel(Q.Cmd) is
						when Cancel_Failed =>
							raise Program_Error;
						when Cancel_Succeeded =>
							null;
						when Cancel_With_Open_Cursor =>
							raise Program_Error;
					end case;
			end case;

			Q.Cmd := CT_Lib_Release(Q.Cmd);
			if Q.Cmd /= Null_Command then
				raise Program_Error;
			end if;
		end if;

		Name_Generator.Generate(Q,Q.Cursor_Name);		-- Generate a new name (helps with debugging)
		Q.Results := No_Results;				-- No results available now
	end Clear_Results;



	function Is_Column(Q : Query_Type; CX : Column_Index_Type) return Boolean is
		Cols : Natural := Columns(Q);
	begin
		return Natural(CX) >= 1 and then Natural(CX) <= Cols;
	end Is_Column;



	--
	-- INTERNAL : Convert application column index to physical index 
	--
	function To_Physical_Column_Index(Q : Query_Type; Logical_Index : Column_Index_Type) return Column_Index_Type is
		Col_X : Natural := 0;
	begin
		pragma assert(Q.Values /= null);
		pragma assert(Logical_Index >= 1);

		for X in Q.Values'Range loop
			if ( Q.Values(X).Status and CS_HIDDEN ) = 0 then
				-- This is not a hidden column
				Col_X := Col_X + 1;
				if Logical_Index = Column_Index_Type(Col_X) then
					return X;
				end if;
			end if;
		end loop;
		raise Program_Error;
	end To_Physical_Column_Index;



	-- END INTERNAL 



	procedure Clear(Q : in out Query_Type) is
	begin
		Clear_Results(Q);
		Clear(Root_Query_Type(Q));
		Q.Row_ID := Null_Row_ID;
	end Clear;



	procedure Append(Q : in out Query_Type; V : APQ_Boolean; After : String := "") is
	begin
		if V = True then
			Append(Q,"1",After);
		else
			Append(Q,"0",After);
		end if;
	end Append;



	procedure Append_Quoted(Q : in out Query_Type; Connection : Root_Connection_Type'Class; SQL : String; After : String := "") is

		function Quote_String(S : String; Quote_Character : Character) return String is
			use Ada.Strings.Unbounded;
			R : Unbounded_String;
		begin
			for X in S'Range loop
				if S(X) = Quote_Character then
					Append(R,S(X));		-- Double up
				end if;
				Append(R,S(X));
			end loop;
			return To_String(R);
		end Quote_String;

	begin
		Append(Q,"'",Quote_String(SQL,'''));
		Q.Caseless(Q.Count) := False;			-- Preserve the case of the above
		Append(Q,"'",After);
	end Append_Quoted;



	procedure Set_Fetch_Mode(Q : in out Query_Type; Mode : Fetch_Mode_Type) is
	begin

		if Q.Cmd /= Null_Command or else Q.Results /= No_Results then
			Raise_Exception(Failed'Identity,
				"SY47: Cannot change fetch mode when results exist (Set_Fetch_Mode).");
		end if;

		case Mode is
			when Sequential_Fetch | Cursor_For_Update | Cursor_For_Read_Only =>
				Set_Fetch_Mode(Root_Query_Type(Q),Mode);
			when Random_Fetch =>
				Raise_Exception(Not_Supported'Identity,
					"SY48: Random_Fetch mode is not yet supported for CT_Lib in APQ (Set_Fetch_Mode).");
		end case;

	end Set_Fetch_Mode;



	procedure Execute(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
	begin

		Query.SQL_Case := Connection.SQL_Case;

		if not Is_Connected(Connection_Type(Connection)) then
			Raise_Exception(Not_Connected'Identity,
				"SY12: The Connection_Type object is not connected (Execute).");
		end if;

		Clear_Results(Query);
		Query.Rewound := True;
		Query.Columns := 0;

		-- Cheats that are needed at the moment :
		Query.SQLCA := Connection_Type(Connection).SQLCA;		-- Share same SQLCA
		Clear_SQLCA(Query.SQLCA.all);


		declare
			use Interfaces.C;
			SQL : String := To_String(Query);
		begin
			if Connection_Type(Connection).Trace_On then
				if Connection_Type(Connection).Trace_Mode = Trace_APQ
				or Connection_Type(Connection).Trace_Mode = Trace_Full then
					Ada.Text_IO.New_Line(Connection.Trace_Ada);
					Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- SQL QUERY:");
					Ada.Text_IO.Put_Line(Connection.Trace_Ada,SQL);
					Ada.Text_IO.Put_Line(Connection.Trace_Ada,";");
					Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Fetch mode: " & Fetch_Mode_Type'Image(Query.Mode));
				end if;
			end if;

			Connection_Type(Connection).SQLCA.Post_Once := True;	-- Post only the first occuring error
			Connection_Type(Connection).SQLCA.Posted := False;	-- Accept new error information

			case Query.Mode is
				when Sequential_Fetch =>
					Query.Cmd := CT_Lib_Exec(Connection_Type(Connection).Connection,SQL);
				when Random_Fetch =>
					raise Program_Error;			-- Set_Fetch_Mode should disallow this anyway
				when Cursor_For_Update =>
					if Connection_Type(Connection).Trace_On then
						Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Cursor:  " & String(Query.Cursor_Name));
					end if;
					Query.Cmd := CT_Lib_Open_Cursor(Connection_Type(Connection).Connection,SQL,
						Query.Cursor_Name'Address,Query.Cursor_Name'Length,For_Update => True);
				when Cursor_For_Read_Only =>
					if Connection_Type(Connection).Trace_On then
						Ada.Text_IO.Put_Line(Connection.Trace_Ada,"-- Cursor:  " & String(Query.Cursor_Name));
					end if;
					Query.Cmd := CT_Lib_Open_Cursor(Connection_Type(Connection).Connection,SQL,
						Query.Cursor_Name'Address,Query.Cursor_Name'Length,For_Update => False);
			end case;
		end;

		if Query.Cmd /= Null_Command then
			-- Successful in launching the query
			Query.Tuple_Index := Tuple_Index_Type'First;

			Connection_Type(Connection).SQLCA.Post_Once := True;		-- Post only the first occuring error
			Connection_Type(Connection).SQLCA.Posted := False;		-- Accept new error information

			CT_Lib_Results(Query.Cmd,Query.Results,Query.Columns);
			Query.SQLCA := Connection_Type(Connection).SQLCA;

			if Connection.Trace_On then
				Report_Error(Connection_Type(Connection).Trace_Ada,Query.SQLCA.all);
				Ada.Text_IO.Put_Line(Connection_Type(Connection).Trace_Ada,
					"-- Results : " & Result_Type'Image(Query.Results));
			end if;

			case Query.Results is
				when Execution_Failed =>
					Raise_Exception(SQL_Error'Identity,
						"SY13: The execution of the query failed (Execute).");
				when No_Results =>
					if Is_Select(Query) then	-- Is this test really needed now???
						Raise_Exception(SQL_Error'Identity,
							"SY14: No results were returned from a SELECT (Execute).");
					elsif CT_Lib_Is_End(Query.Cmd) then
						if Is_Insert(Query) then
							--
							-- Last successful SQL statement was an INSERT statement. We
							-- must pick up the @@identity now, in case the user wants to
							-- call Command_OID() to get the identity of this insert. The
							-- Command_OID() function does not get to use the connection,
							-- so this must be done now while we have that argument given.
							--
							declare
								Q : Query_Type;
							begin
								Prepare(Q,"SELECT @@IDENTITY");
								Execute(Q,Connection);
								Fetch(Q);
								Query.Row_ID := Value(Q,1);	-- Put @@identity into Row_ID
								Clear(Q);
							exception
								when SQL_Error =>
									Query.Row_ID := Null_Row_ID;
									Raise_Exception(Failed'Identity,
										"SY15: Unable to query the @@identity value after INSERT (Execute).");
							end;
						end if;
					else
						Raise_Exception(SQL_Error'Identity,
							"SY16: SQL Query failed (Execute).");
					end if;
				when Info_Results =>
					raise Program_Error;			-- Should this ever happen in APQ???
				when Row_Results | Cursor_Results =>
					if Connection.Trace_On then
						Ada.Text_IO.Put_Line(Connection_Type(Connection).Trace_Ada,
							"-- Columns in result:" & Natural'Image(Query.Columns));
					end if;
					Query.Values := CT_Lib_Describe(Query.Cmd,Query.Columns);
				when Compute_Results | Param_Results | Status_Results =>
					raise Program_Error;			-- These should not be seen here
			end case;

		else
			-- Query failed :
			Query.SQLCA := Connection_Type(Connection).SQLCA;

			if Connection.Trace_On then
				Report_Error(Connection_Type(Connection).Trace_Ada,Query.SQLCA.all);
				Ada.Text_IO.Put_Line(Connection_Type(Connection).Trace_Ada,
					"-- Query Failed to Execute");
			end if;

			Raise_Exception(SQL_Error'Identity,
				"SY17: Execution of query failed (Execute).");
		end if;

	end Execute;



	procedure Execute_Checked(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class; Msg : String := "") is
		use Ada.Text_IO;
	begin

		begin
			Execute(Query,Connection);
		exception
			when Ex : SQL_Error =>
				if Msg'Length > 0 then
					Put(Standard_Error,"*** SQL ERROR: ");
					Put_Line(Standard_Error,Msg);
				else
					Put(Standard_Error,"*** SQL ERROR IN QUERY:");
					New_Line(Standard_Error);
					Put(Standard_Error,To_String(Query));
					if Col(Standard_Error) > 1 then
						New_Line(Standard_Error);
					end if;
				end if;
				Put(Standard_Error,"[");
				Put(Standard_Error,Result_Type'Image(Result(Query)));
				Put(Standard_Error,": ");
				Put(Standard_Error,Error_Message(Query));
				Put_Line(Standard_Error,"]");
				Reraise_Occurrence(Ex);
			when Ex : others =>
				Reraise_Occurrence(Ex);
		end;

	end Execute_Checked;



	procedure Begin_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
		Saved_Mode : Fetch_Mode_Type := Query.Mode;
	begin
		Clear(Query);
		Set_Fetch_Mode(Query,Sequential_Fetch);
		Prepare(Query,"BEGIN TRANSACTION");
		begin
			Execute(Query,Connection_Type(Connection));
		exception
			when Ex : SQL_Error =>
				Set_Fetch_Mode(Query,Saved_Mode);
				Reraise_Occurrence(Ex);
		end;
		Clear(Query);
		Set_Fetch_Mode(Query,Saved_Mode);
	end Begin_Work;



	procedure Commit_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
		Saved_Mode : Fetch_Mode_Type := Query.Mode;
	begin
		Clear(Query);
		Set_Fetch_Mode(Query,Sequential_Fetch);
		Prepare(Query,"COMMIT");
		begin
			Execute(Query,Connection_Type(Connection));
		exception
			when Ex : SQL_Error =>
				Set_Fetch_Mode(Query,Saved_Mode);
				Reraise_Occurrence(Ex);
		end;
		Clear(Query);
		Set_Fetch_Mode(Query,Saved_Mode);
	end Commit_Work;
	


	procedure Rollback_Work(Query : in out Query_Type; Connection : in out Root_Connection_Type'Class) is
		Saved_Mode : Fetch_Mode_Type := Query.Mode;
	begin
		Clear(Query);
		Set_Fetch_Mode(Query,Sequential_Fetch);
		Prepare(Query,"ROLLBACK");
		begin
			Execute(Query,Connection_Type(Connection));
		exception
			when Ex : SQL_Error =>
				Set_Fetch_Mode(Query,Saved_Mode);
				Reraise_Occurrence(Ex);
		end;
		Clear(Query);
		Set_Fetch_Mode(Query,Saved_Mode);
	end Rollback_Work;



	procedure Rewind(Q : in out Query_Type) is
	begin
		Raise_Exception(Not_Supported'Identity,
			"SY23: APQ does not yet support random fetch mode (Rewind).");
	end Rewind;



	procedure Fetch(Q : in out Query_Type) is
		R : Fetch_Return_Type;
	begin

		if Q.Values = null then
			Raise_Exception(No_Result'Identity,
				"SY24: There are no row results to fetch (Fetch).");
		end if;

		Free(Q.Values,False);

		if not Q.Rewound then
			Q.Tuple_Index := Q.Tuple_Index + 1;
		else
			Q.Rewound := False;
			Q.Tuple_Index := First_Tuple_Index;
		end if;

		Q.SQLCA.Post_Once := True;
		Q.SQLCA.Posted := False;

		R := CT_Lib_Fetch(Q.Cmd);

		case R is
			when Fetch_Row =>
				CT_Lib_Get_Data(Q.Cmd,Q.Values.all);
			when Fetch_End =>
				if Q.Results = Cursor_Results or else Q.Results = Row_Results then
					if CT_Lib_Is_Done(Q.Cmd) then
						if Q.Results = Cursor_Results then
							if not CT_Lib_Close_Cursor(Q.Cmd) then
								raise Program_Error;
							end if;
						end if;
						Raise_Exception(No_Tuple'Identity,
							"SY25: There are no more rows to fetch (Fetch).");
					else
						Raise_Exception(Failed'Identity,
							"SY26: Unable to fetch a row (Fetch).");
					end if;
				end if;
			when Fetch_Failed =>
				Raise_Exception(Failed'Identity,
					"SY27: Failed to fetch a row (Fetch).");
		end case;

	end Fetch;



	procedure Fetch(Q : in out Query_Type; TX : Tuple_Index_Type) is
	begin
		Raise_Exception(Not_Supported'Identity,
			"SY28: APQ does not yet support random fetch mode for CT_Lib (Fetch).");
	end Fetch;



	function End_of_Query(Q : Query_Type) return Boolean is
	begin
		Raise_Exception(Not_Supported'Identity,
			"SY30: End_Of_Query is not supported for CT_Lib and should be avoided for all (End_of_Query).");
		return True;		-- To quiet the compiler only
	end End_of_Query;



	function Tuple(Q : Query_Type) return Tuple_Index_Type is
		NT : Tuple_Count_Type := Tuples(Q);	-- May raise No_Result
	begin
		if NT < 1 or else Q.Rewound then
			Raise_Exception(No_Tuple'Identity,
				"SY29: There are no rows to fetch (Tuple).");
		end if;
		return Q.Tuple_Index;
	end Tuple;



	function Tuples(Q : Query_Type) return Tuple_Count_Type is
	begin
		if Q.Cmd = Null_Command or else Q.Results /= Row_Results then
			Raise_Exception(No_Result'Identity,
				"SY31: There are no row results (Tuples).");
		end if;
		Raise_Exception(Not_Supported'Identity,
			"SY32: CT_Lib will not report in advance how many rows are being returned (Tuples).");
		return 0;		-- To quiet compiler
	end Tuples;



	function Columns(Q : Query_Type) return Natural is
	begin
		if Q.Cmd = Null_Command or else ( Q.Results /= Row_Results and Q.Results /= Cursor_Results ) then
			Raise_Exception(No_Result'Identity,
				"SY37: There are no results (Columns).");
		end if;
		pragma assert(Q.Values /= null);

		declare
			Cols : Natural := 0;
		begin
			for X in Q.Values'Range loop
				if ( Q.Values(X).Status and CS_HIDDEN ) = 0 then	-- Don't count hidden columns
					Cols := Cols + 1;
				end if;
			end loop;
			return Cols;
		end;
	end Columns;



	function Column_Name(Q : Query_Type; CX : Column_Index_Type) return String is
	begin
		if Q.Cmd = Null_Command or else Q.Values = null then
			Raise_Exception(No_Result'Identity,
				"SY38: There are no results (Column_Name).");
		end if;

		if not Is_Column(Q,CX) then
			Raise_Exception(No_Column'Identity,
				"SY39: There is no column #" & Column_Index_Type'Image(CX) & " (Column_Name).");
		end if;

		declare
			PX : Column_Index_Type := To_Physical_Column_Index(Q,CX);
		begin
			return To_Case(Q.Values.all(PX).Name(1..Q.Values.all(PX).Name_Length),Q.SQL_Case);
		end;
	end Column_Name;



	function Column_Index(Q : Query_Type; Name : String) return Column_Index_Type is
		Cased_Name :	String := To_Case(Name,Q.SQL_Case);
		LX :				Column_Index_Type := 1;
	begin
		if Q.Cmd = Null_Command then
			Raise_Exception(No_Result'Identity,
				"SY40: There are no column results (Column_Index).");
		end if;

		for X in Q.Values'Range loop
			if ( Q.Values(X).Status and CS_HIDDEN ) = 0 then
				-- this is not a hidden column
				if Q.Values(X).Name(1..Q.Values(X).Name_Length) = Cased_Name then
					return LX;
				end if;
				LX := LX + 1;
			end if;
		end loop;

		Raise_Exception(No_Column'Identity,
			"SY41: There is no column named '" & Name & "' (Column_Index).");
		return LX;	-- For compiler only
	end Column_Index;



	function Column_Type(Q : Query_Type; CX : Column_Index_Type) return Field_Type is
	begin

		if Q.Cmd = Null_Command or else Q.Values = null then
			Raise_Exception(No_Result'Identity,
				"SY44: There are no results (Column_Type).");
		end if;

		if not Is_Column(Q,CX) then
			Raise_Exception(No_Column'Identity,
				"SY45: There is no column #" & Column_Index_Type'Image(CX) & " (Column_Type).");
		end if;

		declare
			PX : Column_Index_Type := To_Physical_Column_Index(Q,CX);
		begin
			return Q.Values.all(PX).Data_Type;
		end;

	end Column_Type;



	function Is_Null(Q : Query_Type; CX : Column_Index_Type) return Boolean is
	begin

		if Q.Cmd = Null_Command or else Q.Values = null then
			Raise_Exception(No_Result'Identity,
				"SY42: There are no column results (Is_Null).");
		end if;

		if not Is_Column(Q,CX) then
			Raise_Exception(No_Column'Identity,
				"SY43: There is no column #" & Column_Index_Type'Image(CX) & " (Is_Null).");
		end if;

		declare
			PX : Column_Index_Type := To_Physical_Column_Index(Q,CX);
		begin
			return Q.Values.all(PX).Str_Value = null;
		end;

	end Is_Null;



	function Value(Query : Query_Type; CX : Column_Index_Type) return String is
	begin
		if not Is_Column(Query,CX) then
			Raise_Exception(No_Column'Identity,
				"SY19: No such column #" & Column_Index_Type'Image(CX) & " (Value).");
		end if;

		if Query.Cmd = Null_Command then
			Raise_Exception(No_Result'Identity,
				"SY20: There was no command executed (Value).");
		end if;

		if Query.Values = null then
			Raise_Exception(No_Result'Identity,
				"SY21: There are no results (Value).");
		end if;

		declare
			use Ada.Strings, Ada.Strings.Fixed;

			PX : Column_Index_Type := To_Physical_Column_Index(Query,CX);
		begin
			if Query.Values.all(PX).Str_Value /= null then
				return Trim(Query.Values.all(PX).Str_Value.all,Right);
			else
				Raise_Exception(Null_Value'Identity,
					"SY22: Column #" & Column_Index_Type'Image(CX) & " is NULL (Value).");
			end if;
		end;
	end Value;



	function Result(Query : Query_Type) return Natural is
	begin
		return Result_Type'Pos(Result(Query));
	end Result;



	function Result(Query : Query_Type) return Result_Type is
	begin
		return Query.Results;
	end Result;



	function Command_Oid(Query : Query_Type) return Row_ID_Type is
	begin
		return Query.Row_ID;
	end Command_Oid;



	function Null_Oid(Query : Query_Type) return Row_ID_Type is
	begin
		Raise_Exception(Not_Supported'Identity,
			"SY46: CT_Lib does not support row id values (Null_Oid).");
		return Row_ID_Type'First;		-- For compiler only
	end Null_Oid;



	function Error_Message(Query : Query_Type) return String is
	begin
		if Query.SQLCA /= null then
			if Query.SQLCA.Sqlerrm /= null then
				return Query.SQLCA.Sqlerrm.all;
			else
				return "";
			end if;
		else
			return "";
		end if;
	end Error_Message;



	function Is_Duplicate_Key(Query : Query_Type) return Boolean is
	begin
		if Query.SQLCA = null then
			return False;
		elsif Query.SQLCA.SQLCODE = -2601 then
			return True;
		else
			return False;
		end if;
	end Is_Duplicate_Key;



	function Engine_Of(Q : Query_Type) return Database_Type is
	begin
		return Engine_CT_Lib;
	end Engine_Of;



	function Cursor_Name(Query : Query_Type) return String is
	begin
		if Query.Results = Cursor_Results then
			return String(Query.Cursor_Name);
		else
			Raise_Exception(No_Result'Identity,
				"SY49: There are no cursor results (Cursor_Name).");
		end if;
	end Cursor_Name;



	function SQL_Code(Query : Query_Type) return SQL_Code_Type is
	begin
		if Query.SQLCA /= null then
			return Query.SQLCA.Sqlcode;
		else
			Raise_Exception(No_Result'Identity,
				"SY33: There are no results to report a SQL Code from (SQL_Code).");
		end if;
	end SQL_Code;





	--
	-- INTERNAL OTHERS
	--



	--
	-- This is the Client Error callback (from c_ct_lib.c)
	--
	procedure CT_Lib_Client_CB(
		Connection :		System.Address;
		Message_Layer :		APQ.CT_Lib.Layer_Type;
		Message_Origin :	APQ.CT_Lib.Origin_Type;
		Message_Severity :	APQ.CT_Lib.Severity_Type;
		Message_Number :	APQ.CT_Lib.Message_Number_Type;
		Message :		Interfaces.C.Strings.chars_ptr;
		Message_Length :	APQ.CT_Lib.Int_Type;
		OS_Message :		Interfaces.C.Strings.chars_ptr;
		OS_Message_Length :	APQ.CT_Lib.Int_Type
	) is
		use Ada.Text_IO;
		Conn_Ptr : Conn_Addr.Object_Pointer := Conn_Addr.To_Pointer(CT_Lib_Client_CB.Connection);
	begin

		if Message_Number = 155 then
			return;		-- Ignore this stupid message:
					-- "ct_results(): user api layer: external error: " &
					-- "This routine cannot be called when the command structure is idle."
		end if;

		if Conn_Ptr.Trace_On then
			New_Line(Conn_Ptr.Trace_Ada);
			Put_Line(Conn_Ptr.Trace_Ada,"--");
			Put_Line(Conn_Ptr.Trace_Ada,"-- Client Error Message:");
			Put_Line(Conn_Ptr.Trace_Ada,"--      Layer:" & APQ.CT_Lib.Layer_Type'Image(Message_Layer));
			Put_Line(Conn_Ptr.Trace_Ada,"--     Origin:" & APQ.CT_Lib.Origin_Type'Image(Message_Origin));
			Put_Line(Conn_Ptr.Trace_Ada,"--   Severity:" & APQ.CT_Lib.Severity_Type'Image(Message_Severity));
			Put_Line(Conn_Ptr.Trace_Ada,"--     Number:" & APQ.CT_Lib.Message_Number_Type'Image(Message_Number));
			Put_Line(Conn_Ptr.Trace_Ada,"--    Message: " & Value_Of(Message));
			if OS_Message_Length > 0 then
				Put_Line(Conn_Ptr.Trace_Ada,"-- O/S Message: " & Value_Of(OS_Message));
			end if;
			Put_Line(Conn_Ptr.Trace_Ada,"--");
			Flush(Conn_Ptr.Trace_Ada);
		end if;

		if Conn_Ptr.SQLCA.Post_Once = False or Conn_Ptr.SQLCA.Posted = False then
			Conn_Ptr.SQLCA.Sqlcode := -SQL_Code_Type(Message_Number);
			Replace_String(Conn_Ptr.SQLCA.Sqlerrm,Value_Of(Message));
			Conn_Ptr.SQLCA.Sqlerrp := (' ', others => ' ');
			Conn_Ptr.SQLCA.Rows_Affected := 0;
			Conn_Ptr.SQLCA.Sqlwarn := (' ', others => ' ');
			Conn_Ptr.SQLCA.Posted := True;
		end if;
	end CT_Lib_Client_CB;



	procedure CT_Lib_Server_CB(
		Connection :		System.Address;
		Message_Severity :	APQ.CT_Lib.Severity_Type;
		Message_Number :	APQ.CT_Lib.Message_Number_Type;
		State :			APQ.CT_Lib.State_Type;
		Line :			APQ.CT_Lib.Line_Type;
		Server_Name :		Interfaces.C.Strings.chars_ptr;
		Server_Name_Length :	APQ.CT_Lib.Int_Type;
		Proc_Name :		Interfaces.C.Strings.chars_ptr;
		Proc_Name_Length :	APQ.CT_Lib.Int_Type;
		Message :		Interfaces.C.Strings.chars_ptr
	) is
		use Ada.Text_IO;
		Conn_Ptr : Conn_Addr.Object_Pointer := Conn_Addr.To_Pointer(CT_Lib_Server_CB.Connection);
	begin
		if Conn_Ptr.Trace_On then
			New_Line(Conn_Ptr.Trace_Ada);
			Put_Line(Conn_Ptr.Trace_Ada,"--");
			Put_Line(Conn_Ptr.Trace_Ada,"-- Server Error Message:");
			Put_Line(Conn_Ptr.Trace_Ada,"--   Severity:" & APQ.CT_Lib.Severity_Type'Image(Message_Severity));
			Put_Line(Conn_Ptr.Trace_Ada,"--     Number:" & APQ.CT_Lib.Message_Number_Type'Image(Message_Number));
			Put_Line(Conn_Ptr.Trace_Ada,"--      State:" & APQ.CT_Lib.State_Type'Image(State));
			Put_Line(Conn_Ptr.Trace_Ada,"--       Line:" & APQ.CT_Lib.Line_Type'Image(Line));
			if Server_Name_Length > 0 then
				Put_Line(Conn_Ptr.Trace_Ada,"-- Server Name: " & Value_Of(Server_Name));
			end if;
			if Proc_Name_Length > 0 then
				Put_Line(Conn_Ptr.Trace_Ada,"--   Proc Name: " & Value_Of(Proc_Name));
			end if;
			Put_Line(Conn_Ptr.Trace_Ada,"--     Message: " & Value_Of(Message));
			Put_Line(Conn_Ptr.Trace_Ada,"--");
			Flush(Conn_Ptr.Trace_Ada);
		end if;

		if Conn_Ptr.SQLCA.Post_Once = False or Conn_Ptr.SQLCA.Posted = False then
			Conn_Ptr.SQLCA.Sqlcode := -SQL_Code_Type(Message_Number);
			Replace_String(Conn_Ptr.SQLCA.Sqlerrm,Value_Of(Message));
			if Proc_Name_Length > 0 then
				declare
					Procedure_Name : String := Value_Of(Proc_Name);
					Length : Natural := Conn_Ptr.SQLCA.Sqlerrp'Length;
				begin
					if Procedure_Name'Length < Length then
						Length := Procedure_Name'Length;
					end if;
					Conn_Ptr.SQLCA.Sqlerrp := (' ', others => ' ');
					Conn_Ptr.SQLCA.Sqlerrp(1..Length) := Procedure_Name(1..Length);
				end;
			else
				Conn_Ptr.SQLCA.Sqlerrp := (' ', others => ' ');
			end if;
			Conn_Ptr.SQLCA.Rows_Affected := 0;
			Conn_Ptr.SQLCA.Sqlwarn := (' ', others => ' ');
			Conn_Ptr.SQLCA.Posted := True;
		end if;

		if Message_Number = CT_Lib_Change_Database_Context then
			declare
				Prefix : constant String := "Changed database context to '";
			begin
				if Conn_Ptr.SQLCA.Sqlerrm'Length > Prefix'Length
				and then Conn_Ptr.SQLCA.Sqlerrm(1..Prefix'Length) = Prefix then
					declare
						Database_Name : String := Conn_Ptr.SQLCA.Sqlerrm(Prefix'Length+1..Conn_Ptr.SQLCA.Sqlerrm'Last);
						X : Natural := Ada.Strings.Fixed.Index(Database_Name,"'");
					begin
						if X > 0 then
						--	Set_DB_Name(Root_Connection_Type(Conn_Ptr.all),Database_Name(Database_Name'First..X-1));
							null;
						end if;
					end;
				end if;
			end;
		end if;
	end CT_Lib_Server_CB;



	function Null_Oid return Row_ID_Type is
	begin
		return Null_Row_ID;
	end Null_Oid;




-- private




	procedure Finalize(C : in out Connection_Type) is
	begin
		Internal_Reset(C,True);
		if C.Connection /= Null_Connection then
			CT_Lib_Free_Connection(C.Connection);
		end if;
		if C.Context /= Null_Context then
			CT_Lib_Free_Context(C.Context);
		end if;
		if C.SQLCA /= null then
			Free(C.SQLCA);
		end if;
	end Finalize;



	procedure Initialize(C : in out Connection_Type) is
	begin
		C.SQL_Case	:= Lower_Case;			-- Lowercase all SQL text
		C.Context	:= CT_Lib_Alloc_Context;		-- CS_CONTEXT
		if C.Context = Null_Context then
			Raise_Exception(Failed'Identity,
				"SY10: Failed to initialize Connection_Type (Check LANG environment variable).");
		end if;
		C.Connection := CT_Lib_Alloc_Connection(C.Context,C'Address); -- CS_CONNECTION
		if C.Connection = Null_Connection then
			Raise_Exception(Failed'Identity,
				"SY11: Failed to allocate a CT_Lib connection object (Check environment).");
		end if;
		C.Connected	:= False;
		C.SQLCA		:= new SQLCA_Type;		-- SQL Communications Area
		
	end Initialize;



	function query_factory(C: in Connection_Type) return Root_Query_Type'Class is
		Q: Query_Type;
	begin
		return q;
	end query_factory;



	procedure Initialize(Q : in out Query_Type) is
	begin
		Initialize(Root_Query_Type(Q));
		Q.SQL_Case := Lower_Case;			-- Lowercase all SQL query text for CT_Lib
		Q.Mode := Sequential_Fetch;			-- By default, assume efficient mode
		Q.Row_ID := Null_Row_ID;
		Name_Generator.Generate(Q,Q.Cursor_Name);	-- Generate a cursor name
	end Initialize;



	procedure Adjust(Q : in out Query_Type) is
	begin
		Q.SQLCA	:= null;				-- Do not share connection's SQLCA either
		Q.Results := No_Results;			-- Do not share in the results
		Adjust(Root_Query_Type(Q));
		-- Shares Q.SQLCA until another call with a connect parameter is used
	end Adjust;



	procedure Finalize(Q : in out Query_Type) is
	begin
		begin
			Clear(Q);
		exception
			when Failed | Program_Error =>
				if Q.Cmd /= Null_Command then
					Q.Cmd := CT_Lib_Release(Q.Cmd);
					if Q.Cmd /= Null_Command then
						Q.Cmd := Null_Command;
					end if;
				end if;
			Q.Results := No_Results;		-- No results available now
		end;

		Q.SQLCA := null;				-- Forget the cheat ptr for the SQLCA in the Connection_Type object
	end Finalize;



	procedure Set_Client_CB(Proc : Client_Msg_CB) is
		procedure CT_Lib_Reg_ClientMsgCB(Proc : Client_Msg_CB);
		pragma import(C,CT_Lib_Reg_ClientMsgCB,"c_ct_lib_reg_clientmsgcb");
	begin
		CT_Lib_Reg_ClientMsgCB(Proc);
	end Set_Client_CB;



	procedure Set_Server_CB(Proc : Server_Msg_CB) is
		procedure CT_Lib_Reg_ServerMsgCB(Proc : Server_Msg_CB);
		pragma import(C,CT_Lib_Reg_ServerMsgCB,"c_ct_lib_reg_servermsgcb");
	begin
		CT_Lib_Reg_ServerMsgCB(Proc);
	end Set_Server_CB;



begin

	Set_Client_CB(CT_Lib_Client_CB'Access);
	Set_Server_CB(CT_Lib_Server_CB'Access);

end APQ.CT_Lib.Client;

