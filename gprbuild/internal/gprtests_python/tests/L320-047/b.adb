with A;

package body B is 

  procedure B_Stuff (Anything: in Anything_Type;
                     Something_Else : out Anything_Type) is

  begin
      A.A_Stuff;
      Something_Else := Anything;
  end B_Stuff;

end B;
