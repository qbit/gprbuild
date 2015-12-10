pragma Ada_05;
with Ada.Containers.Indefinite_Vectors;
package DDS.InstanceHandle_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, DDS.InstanceHandle_T, DDS_Support."=");
