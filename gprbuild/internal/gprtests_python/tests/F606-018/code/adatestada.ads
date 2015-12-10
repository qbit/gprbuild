with System;

package AdaTestAda is
   type Object is record
      m_flag : Boolean := False;
   end record;
   
   procedure Configure;

   procedure Update;

   pragma export(C, Configure, "AdaTestAda_Configure");                    
   pragma export(C, Update, "AdaTestAda_Update");
end AdaTestAda;