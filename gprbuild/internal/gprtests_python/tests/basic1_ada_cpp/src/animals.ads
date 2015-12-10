with Interfaces.C.Strings; use Interfaces.C.Strings;
package Animals is
   package Ifaces is
      --  --------------------------------------------------------------------
      type Carnivore is limited interface;
      function Number_Of_Teeth (X : Carnivore) return Natural is abstract;
      pragma Convention (C_Plus_Plus, Number_Of_Teeth); --  Required by AI-430

      --  --------------------------------------------------------------------
      type Domestic is limited interface;
      procedure Set_Owner (X : in out Domestic; Name : Chars_Ptr) is abstract;
      pragma Convention (C_Plus_Plus, Set_Owner);
   end Ifaces;
   use Ifaces;

   --  -----------------------------------------------------------------------
   type Animal is tagged limited record
      Age : Natural;
   end record;
   pragma Import (C_Plus_Plus, Animal);

   procedure Set_Age (X : in out Animal; Age : Natural);
   pragma Import (C_Plus_Plus, Set_Age);

   function Age (X : Animal) return Natural;
   pragma Import (C_Plus_Plus, Age);

   function New_Animal return Animal;
   pragma CPP_Constructor (New_Animal);
   pragma Import (CPP, New_Animal, "_ZN6AnimalC2Ev");
   --  We must import the constructor from C++ since all the primitives
   --  are defined in C++ (and hence the C++ constructor is responsible
   --  of building the dispatch tables).

   --  -----------------------------------------------------------------------
   type Dog is new Animal and Carnivore and Domestic with record
      Tooth_Count : Natural;
      Owner       : String (1 .. 30);
   end record;
   pragma Import (C_Plus_Plus, Dog);

   function Number_Of_Teeth (A : Dog) return Natural;
   pragma Import (C_Plus_Plus, Number_Of_Teeth);

   procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);
   pragma Import (C_Plus_Plus, Set_Owner);

   function New_Dog return Dog'Class;
   pragma CPP_Constructor (New_Dog);
   pragma Import (C_Plus_Plus, New_Dog, "_ZN3DogC2Ev");

   --  -----------------------------------------------------------------------
   --  Example of a type derivation defined in the Ada side that inherites
   --  all the dispatching primitives of the ancestor from the C++ side.

   type Vaccinated_Dog is new Dog with null record;
   function Vaccination_Expired (A : Vaccinated_Dog) return Boolean;
   pragma Convention (C_Plus_Plus, Vaccination_Expired); 
end Animals;
