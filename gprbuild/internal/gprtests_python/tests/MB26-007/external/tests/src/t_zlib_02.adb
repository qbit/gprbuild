with Prime.Dates; use Prime.Dates;
with Prime.Randoms_New; use Prime.Randoms_New;
with Prime.Heap; use Prime.Heap;
with PRIME.Logs; use PRIME.Logs;
with Prime.Types; use Prime.Types;
with Prime.Base64; use Prime.Base64;
with Ada.Unchecked_Conversion; 
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization; use Ada.Finalization;
with Prime.Variants; use Prime.Variants;
with Prime.Variants_Misc; use Prime.Variants_Misc;
with Prime.Tools; use Prime.Tools;

procedure T_ZLib_02 is
	procedure C_Exit (Status : Integer);
        pragma Import (C, C_Exit, "exit");

	procedure Compare(N1 : in Num_Vector;N2 : in Num_Vector)
	is
	begin
		Log_Put_Line("Size initial :" & Nat'Image(N1'Last) & " Size Final : " & Nat'Image(N2'Last));
                for I in N1'Range loop
                        if N1(I)=N2(I) then
                                 Log_Put_Line(Nat'Image(I) & ":" & Num'Image(N1(I)) & ":" & Num'Image(N2(I)));
                        else
                                 Log_Put_Line(Nat'Image(I) & ":" & Num'Image(N1(I)) & ":" & Num'Image(N2(I)) & " NOK");
                        end if;
                end loop;
                if N1=N2 then
                        Log_Put_Line("OK : Equals");
                else
                       Log_Put_Line("NOK");
			C_Exit(1);
                end if;
	end Compare;

	Nv : Num_Vector(1..1000);
	--Nv : Num_Vector(1..5);
	Rnd : Random_State;

begin
	--Nv(1):=0.6326; 
        --Nv(2):=0.5769;
        --Nv(3):=0.4498;
        --Nv(4):=0.686;
        --Nv(5):=0.7622;
	for I in 1..Nv'Last loop
		 Get_U (State   => Rnd,Kind    => Enum_Random_Crypto_2,Uniform =>Nv(I));
	end loop;
	Log_Put_Line ("Start");
	Log_Put_Line("++++++++++++++++++++++++");
	Log_Put_Line("Now Testing BASE64 Encode Decode For Num_Vector");
	declare
		Binary_Data 	: Stream_Element_Array	:=Num_Vector_To_Stream(Nv);
		Encoded_Data	: Variant 		:=Encode_Stream_To_Base64(Binary_Data);
		Decoded 	: Stream_Element_Array 	:=Decode_Base64_To_Stream(Encoded_Data);
		V       	: Num_Vector 		:= Stream_To_Num_Vector(Decoded);
	begin
		Compare(Nv,V);
	end;
	Log_Put_Line ("End");
end T_ZLib_02;

