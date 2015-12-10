pragma Ada_05;
with Mig.Rtps;
package DDS.Infrastructure.InstanceHandle is

   type Ref is record
      Guid    : Mig.Rtps.Guid;
      IsValid : Boolean;
   end record;
   pragma Convention (C, Ref);


end DDS.Infrastructure.InstanceHandle;
