with Text_IO; use Text_IO;
with Addresses; use Addresses;
procedure STANS_Tactical is
begin
   Put_Line ("STANS Tactical started");
   Addresses.Set_Path ( "../dat" );
   Addresses.Initialize ( True );
end;
