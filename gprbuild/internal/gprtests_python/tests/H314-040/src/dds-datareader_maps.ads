pragma Ada_05;
with Ada.Containers.Indefinite_Ordered_Maps;
with DDS.DataReader_Impl; use DDS.DataReader_Impl;
package DDS.DataReader_Maps is  new Ada.Containers.Indefinite_Ordered_Maps
  (Standard.String,
   DDS.DataReader_Impl.Ref_Access);
