pragma Ada_05;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Tags;
with RTI.Obj_Impl;
use RTI.Obj_Impl;

package DDS.Tag_Maps is new Ada.Containers.Indefinite_Ordered_Maps
  (Standard.String, RTI.Obj_Impl.Ref_Access);
