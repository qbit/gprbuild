pragma Ada_05;
package Mig.Rtps is
   RTPS_HEADER_SIZE  : constant := 16;

   SUBMESSAGE_HEADER_SIZE : constant := 4;
   SUBMESSAGE_OCTETS_TO_NEXT_HEADER_MAX : constant := 16#FFFF#;
   SUBMESSAGE_SIZE_MIN : constant := 8;
   SUBMESSAGE_HEADER_SIZE_MAX : constant := 64;
   SUBMESSAGE_ALIGNMENT : constant := 4;

   IP_ADDRESS_INVALID : constant := 0;
   PORT_INVALID      : constant := 0;
   TIMESTAMP_SEC_DEFAULT : constant := 0;
   TIMESTAMP_FRAC_DEFAULT : constant := 0;
   PATHNAME_LEN_MAX  : constant := 255;

   type HostId is mod 2 ** 32;
   HOST_ID_UNKNOWN   : constant HostId := 16#0000_0000#;

   type AppId is mod 2 ** 32;
   APP_ID_UNKNOWN    : constant AppId := 16#0000_0000#;

   type ObjectId is mod 2 ** 32;
   OBJECT_ID_UNKNOWN : constant ObjectId := 16#0000_0000#;

   type Guid is  record
      HostId   : Rtps.HostId;
      AppId    : Rtps.AppId;
      ObjectId : Rtps.ObjectId;
   end record;
   pragma Convention (C, Guid);
   Null_Guid : constant Guid :=
                 (HOST_ID_UNKNOWN,
                  APP_ID_UNKNOWN,
                  OBJECT_ID_UNKNOWN);
   function Image (This : in Guid) return Standard.String;
end Mig.Rtps;
