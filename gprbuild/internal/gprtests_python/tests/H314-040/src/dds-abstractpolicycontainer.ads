pragma Ada_05;
with DDS.Struct;
package DDS.AbstractPolicyContainer is
   type Ref is abstract new DDS.Struct.Ref with null record;
end DDS.AbstractPolicyContainer;
