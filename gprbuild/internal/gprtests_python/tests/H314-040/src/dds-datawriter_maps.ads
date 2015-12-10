pragma Ada_05;
with Ada.Containers.Indefinite_Ordered_Maps;
with DDS.DataWriter_Impl; use DDS.DataWriter_Impl;
package DDS.DataWriter_Maps is  new Ada.Containers.Indefinite_Ordered_Maps
  (Standard.String,
   DDS.DataWriter_Impl.Ref_Access);
