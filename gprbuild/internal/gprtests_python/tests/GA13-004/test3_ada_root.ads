package Test3_Ada_Root is
   type Object_Type is abstract tagged limited record
      State : Integer;
   end record;
   pragma Import(Cpp, Object_Type);

   function Constructor return Object_Type'class;
   pragma CPP_Constructor(Constructor);
   pragma Import(CPP, Constructor, "_ZN17Test3_Common_rootC2Ev");
end Test3_Ada_Root;
