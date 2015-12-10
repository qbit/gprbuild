pragma Ada_05;
with DDS.Object;
package DDS.CacheAccess is
   type Ref is new DDS.Object.Ref with null record;
   function Constructor (Params : not null access Integer) return Ref;
end DDS.CacheAccess;
