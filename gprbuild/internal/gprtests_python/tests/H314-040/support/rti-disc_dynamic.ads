pragma Ada_05;

with Interfaces;

package RTI.Disc_Dynamic is
   pragma Preelaborate;

   use type Interfaces.Unsigned_32;

   subtype DISCDynamicProtocolKind is Interfaces.Unsigned_32;
   subtype DISCDynamicProtocolMask is Interfaces.Unsigned_32;

   ENDPOINT_DETECTOR_PROTOCOL : constant DISCDynamicProtocolKind := 2**0;

   ENDPOINT_ANNOUNCER_PROTOCOL : constant DISCDynamicProtocolKind := 2**1;

   PARTICIPANT_SELF_ANNOUNCER_PROTOCOL : constant DISCDynamicProtocolKind := 2**2;

   PARTICIPANT_SELF_DETECTOR_PROTOCOL : constant DISCDynamicProtocolKind := 2**3;

   AUTONOMOUS_PARTICIPANT_PROTOCOL_MASK : constant DISCDynamicProtocolMask :=
     (ENDPOINT_DETECTOR_PROTOCOL or
      ENDPOINT_ANNOUNCER_PROTOCOL or
      PARTICIPANT_SELF_ANNOUNCER_PROTOCOL or
      PARTICIPANT_SELF_DETECTOR_PROTOCOL);

end RTI.Disc_Dynamic;
