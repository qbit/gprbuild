pragma Ada_05;
with Ada.Containers.Indefinite_Vectors;
with RTI.Obj_Impl;
use RTI.Obj_Impl;

package DDS.Vectors is new Ada.Containers.Indefinite_Vectors
  (Natural, RTI.Obj_Impl.Ref_Access);

