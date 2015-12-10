generic
  type Anything_Type is (<>);
package B is
  procedure B_Stuff (Anything: in Anything_Type;
                     Something_Else : out Anything_Type);
end B;
