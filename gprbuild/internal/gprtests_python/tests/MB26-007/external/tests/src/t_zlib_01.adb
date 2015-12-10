with Ada.Text_Io; use Ada.Text_Io;
with Zlib; use Zlib;

procedure T_ZLib_01 is
	Static_Version : constant String := "1.2.3_GPRIME";
	procedure C_Exit (Status : Integer);
   	pragma Import (C, C_Exit, "exit");

begin
	Put_Line ("ZLIB Version:" &  Zlib.Version);
	if Zlib.Version=Static_Version then
		Put_Line("OK");	
		C_Exit(0);
	else
		Put_Line("ERROR : The linked ZLIB Version is not correct. Must be :" & Static_Version);
		C_Exit(1);
	end if;
end T_ZLib_01;

