--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO.Unbounded_IO;
with DDS_Support.Sequences_Generic.Images_Generic;
with System.Address_Image;

package body DDS.Images is
   use Ada.Strings.Unbounded;

   function Image (Index : DDS.Natural; Item  : QosPolicyCount; Indent : Standard.String := "") return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent & Index'Img);
      Append (Ret, "=> (" & Item.Policy_Id'Img & "," & Item.Count'Img & ")");
      return Ret;
   end Image;

   package QosPolicyCount_Images is new QosPolicyCount_Seq.Images_Generic (Image);
   use QosPolicyCount_Images;


   ---------
   -- Put --
   ---------
   function Image
     (Item     : RequestedIncompatibleQosStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent & "Total_Count        :" & Item.Total_Count'Img & ASCII.Lf);
      Append (Ret, Indent & "Total_Count_Change :" & Item.Total_Count'Img & ASCII.Lf);
      Append (Ret, Indent & "Last_Policy_Id     :" & Item.Last_Policy_Id'Img & ASCII.Lf);
      Append (Ret, Indent & "Policies           :" & ASCII.Lf);
      Append (Ret, Image (Item.Policies, Indent & "  "));
      return Ret;
   end Image;

   function Image (Item  : QosPolicyCount; Indent : Standard.String := "")
                   return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent);
      Append (Ret, Item.Policy_Id'Img & ", " & Item.Count'Img);
      return Ret;
   end Image;

   function Image
     (Item     : LivelinessChangedStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent & "Alive_Count             :" & Item.Alive_Count'Img & ASCII.Lf);
      Append (Ret, Indent & "Not_Alive_Count         :" & Item.Not_Alive_Count'Img & ASCII.Lf);
      Append (Ret, Indent & "Alive_Count_Change      :" & Item.Alive_Count_Change'Img & ASCII.Lf);
      Append (Ret, Indent & "Not_Alive_Count_Change  :" & Item.Not_Alive_Count_Change'Img & ASCII.Lf);
      Append (Ret, Indent & "Last_Publication_Handle : <>" & ASCII.Lf);
      return Ret;
   end Image;

   function Image
     (Item     : SampleRejectedStatus; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is

      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent & "Total_Count          :" & Item.Total_Count'Img & ASCII.Lf);
      Append (Ret, Indent & "Total_Count_Change   :" & Item.Total_Count_Change'Img & ASCII.Lf);
      Append (Ret, Indent & "Last_Reason          :" & Item.Last_Reason'Img & ASCII.Lf);
      Append (Ret, Indent & "Last_Instance_Handle : <>" & ASCII.Lf);
      return Ret;
   end Image;

   function Image
     (Item     : DataReaderQos; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Indent & "Durability             :"); Append (Ret, Image (Item.Durability)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Deadline               :"); Append (Ret, Image (Item.Deadline)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Latency_Budget         :"); Append (Ret, Image (Item.Latency_Budget)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Liveliness             :"); Append (Ret, Image (Item.Liveliness)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Reliability            :"); Append (Ret, Image (Item.Reliability)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Destination_Order      :"); Append (Ret, Image (Item.Destination_Order)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "History                :"); Append (Ret, Image (Item.History)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Resource_Limits        :"); Append (Ret, Image (Item.Resource_Limits)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "User_Data              :"); Append (Ret, Image (Item.User_Data)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Ownership              :"); Append (Ret, Image (Item.Ownership)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Time_Based_Filter      :"); Append (Ret, Image (Item.Time_Based_Filter)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Reader_Data_Lifecycle  :"); Append (Ret, Image (Item.Reader_Data_Lifecycle)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Reader_Resource_Limits :"); Append (Ret, Image (Item.Reader_Resource_Limits)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Protocol               :"); Append (Ret, Image (Item.Protocol)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Transport_Selection    :"); Append (Ret, Image (Item.Transport_Selection)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Unicast                :"); Append (Ret, Image (Item.Unicast)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Multicast              :"); Append (Ret, Image (Item.Multicast)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Type_Support           :"); Append (Ret, Image (Item.Type_Support)); Append (Ret, ASCII.Lf);
      Append (Ret, Indent & "Property               :"); Append (Ret, Image (Item.Property)); Append (Ret, ASCII.Lf);
      return Ret;
   end Image;

   function Image
     (Item     : DataWriterQos; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Item, Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin

      return Ret;
   end Image;

   function Image
     (Item     : DurabilityQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img & ", " & Item.Direct_Communication'Img);
      return Ret;
   end Image;

   function Image
     (Item     : DeadlineQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
   begin
      return Image (Item.Period);
   end Image;

   function Image
     (Item     : LatencyBudgetQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
   begin
      return Image (Item.Duration);
   end Image;

   function Image
     (Item     : LivelinessQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
   begin
      return Image (Item.Lease_Duration);
   end Image;

   function Image
     (Item     : ReliabilityQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img & ", " & Image (Item.Max_Blocking_Time));
      return Ret;
   end Image;

   function Image
     (Item     : DestinationOrderQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img & ", " & Item.Scope'Img);
      return Ret;
   end Image;

   function Image
     (Item     : HistoryQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img & ", " & Item.Depth'Img & "," & Item.Refilter'Img);
      return Ret;
   end Image;

   function Image
     (Item     : ResourceLimitsQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Max_Samples'Img &
              ", " & Item.Max_Instances'Img &
              ", " & Item.Max_Samples_Per_Instance'Img &
              ", " & Item.Initial_Samples'Img &
              ", " & Item.Initial_Instances'Img);
      return Ret;
   end Image;

   function Image
     (Item     : UserDataQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<>");
      return Ret;
   end Image;

   function Image
     (Item     : OwnershipQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img);
      return Ret;
   end Image;

   function Image
     (Item     : TimeBasedFilterQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Image (Item.Minimum_Separation));
      return Ret;
   end Image;

   function Image
     (Item     : ReaderDataLifecycleQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Image (Item.Autopurge_Nowriter_Samples_Delay));
      Append (Ret, ",");
      Append (Ret, Image (Item.Autopurge_Disposed_Samples_Delay));
      return Ret;
   end Image;

   function Image
     (Item     : DataReaderResourceLimitsQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : DataReaderProtocolQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : TransportSelectionQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : TransportUnicastQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : TransportMulticastQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : TypeSupportQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin

      Append (Ret, System.Address_Image (Item.Plugin_Data));
      return Ret;
   end Image;

   function Image
     (Item     : PropertyQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, "<TBD>");
      return Ret;
   end Image;

   function Image
     (Item     : ServiceQosPolicy; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Item.Kind'Img);
      return Ret;
   end Image;



   function Image
     (Item     : Duration_T; Indent : Standard.String  := "")
      return Ada.Strings.Unbounded.Unbounded_String is
      pragma Unreferenced (Indent);
      Ret : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Append (Ret, Duration'Image (Duration (Item.Sec) + Duration (Item.Nanosec) * 0.000_0001));
      Append (Ret, " sec");
      return Ret;
   end Image;
end DDS.Images;
