pragma Ada_05;
------------------------------------------------------------------
-- Demo for a skeleton, see skeleton.py in the GPS installation --
------------------------------------------------------------------
with Ada.Finalization;

package DDS.Object is

   type Ref is new Ada.Finalization.Controlled with null record;
   function Constructor (Params : not null access Integer) return Ref;

end DDS.Object;
