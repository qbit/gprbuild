pragma Ada_05;

with DDS.Entity;

package DDS.Domain_Entity is

   type Ref is limited interface and DDS.Entity.Ref;

end DDS.Domain_Entity;
