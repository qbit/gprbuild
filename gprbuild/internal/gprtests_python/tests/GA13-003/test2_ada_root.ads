package Test2_Ada_Root is
   type Root_Type is abstract tagged limited record
      State : Integer;
   end record;

   procedure Execute(X : in out Root_Type) is abstract;

private
   pragma Import (Cpp, Root_Type);
end Test2_Ada_Root;
