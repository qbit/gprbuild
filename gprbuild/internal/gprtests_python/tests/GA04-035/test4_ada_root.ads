package Test4_Ada_Root is
   type Object_Type is abstract tagged limited
      record
         State : Integer;
      end record;
   pragma Import(Cpp, Object_Type);
   type Class_Ptr_Type is access all Object_Type'class;
   procedure Execute(X : in out Object_Type) is abstract;
   --  pragma Import(Cpp, Execute);
 
   function Constructor return Object_Type'class;
   pragma CPP_Constructor(Constructor);
   pragma Import(CPP, Constructor, "_ZN17Test4_Common_rootC2Ev");
end Test4_Ada_Root;
