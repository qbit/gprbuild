with Interfaces.C.Strings; use Interfaces.C.Strings;
package Animals is
   --  -----------------------------------------------------------------------
   type Carnivore is limited interface;
   function Number_Of_Teeth (X : Carnivore) return Natural is abstract;
   pragma Convention (CPP, Number_Of_Teeth); --  Required by AI-430

   --  -----------------------------------------------------------------------
   type Domestic is limited interface;
   procedure Set_Owner (X : in out Domestic; Name : Chars_Ptr) is abstract;
   pragma Convention (CPP, Set_Owner);

   --  -----------------------------------------------------------------------
   type Animal is tagged limited record
      Age : Natural;
   end record;
   pragma Import (CPP, Animal);

   procedure Set_Age (X : in out Animal; Age : Natural);
   pragma Import (CPP, Set_Age);

   function Age (X : Animal) return Natural;
   pragma Import (CPP, Age);

   --  -----------------------------------------------------------------------
   type Dog is new Animal and Carnivore and Domestic with record
      Tooth_Count : Natural;
      Owner       : String (1 .. 30);
   end record;
   pragma Import (CPP, Dog);

   function Number_Of_Teeth (A : Dog) return Natural;

--  Bug in the original sources of this test: in order to fully support
--  renamings it is required that renamings have their own slot (see
--  example filed in 2230-005). Therefore, in order to properly declare
--  primitives following the same order of the C++ declarations, these
--  renamings must be located after the primitives that correspond
--  with the C++ class.
--
--   function Number_Of_Teeth_Renaming (A : Dog) return Natural
--     renames Number_Of_Teeth;
--   pragma Import (CPP, Number_Of_Teeth_Renaming,
--                       "_ZN3Dog15Number_Of_TeethEv");

   procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);

--  New declaration (previously located before declaration of
--  Set_Owner)

   function Number_Of_Teeth_Renaming (A : Dog) return Natural
     renames Number_Of_Teeth;
   pragma Import (CPP, Number_Of_Teeth_Renaming,
                       "_ZN3Dog15Number_Of_TeethEv");

   procedure Set_Owner_Renaming (A : in out Dog; Name : Chars_Ptr)
     renames Set_Owner;
   pragma Import (CPP, Set_Owner_Renaming, "_ZN3Dog9Set_OwnerEPc");

   function New_Dog return Dog'Class;
   pragma CPP_Constructor (New_Dog);
   pragma Import (C_Plus_Plus, New_Dog, "_ZN3DogC2Ev");

   --  -----------------------------------------------------------------------
   --  Example of a type derivation defined in the Ada side that inherites
   --  all the dispatching primitives of the ancestor from the C++ side.

   type Vaccinated_Dog is new Dog with null record;
   function Vaccination_Expired (A : Vaccinated_Dog) return Boolean;
end Animals;
