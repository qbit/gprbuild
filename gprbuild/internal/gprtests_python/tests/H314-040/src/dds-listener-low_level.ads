pragma Ada_05;

with DDS.Listener;
with Unchecked_Conversion;

package DDS.Listener.Low_Level is

   type C_Listener is record
      Listener_Data : System.Address;
   end record;
   pragma Convention (C, C_Listener);

   function C_To_Listener is new Unchecked_Conversion
     (Source => C_Listener, Target => DDS.Listener.Ref_Access);

   function Listener_To_C is new Unchecked_Conversion
     (Source => DDS.Listener.Ref_Access, Target => C_Listener);

end DDS.Listener.Low_Level;
