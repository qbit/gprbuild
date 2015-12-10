with Ada.Strings.Unbounded;
package DDS.Images is

   function Image
     (Item     : RequestedIncompatibleQosStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image (Index : DDS.Natural; Item  : QosPolicyCount; Indent : Standard.String := "")
                   return Ada.Strings.Unbounded.Unbounded_String;

   function Image (Item  : QosPolicyCount; Indent : Standard.String := "")
                   return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : LivelinessChangedStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : SampleRejectedStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : DataReaderQos; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : DataWriterQos; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : DurabilityQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : DeadlineQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

   function Image
     (Item     : LatencyBudgetQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : LivelinessQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : ReliabilityQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : DestinationOrderQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : HistoryQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : ResourceLimitsQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : UserDataQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : OwnershipQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : TimeBasedFilterQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : ReaderDataLifecycleQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : DataReaderResourceLimitsQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : DataReaderProtocolQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : TransportSelectionQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : TransportUnicastQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : TransportMulticastQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : TypeSupportQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : PropertyQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : ServiceQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;
   function Image
     (Item     : Duration_T; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String;

end DDS.Images;
