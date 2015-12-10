

with com.saabgroup.cms.pha.sensor.depth.Depth_DataReader;
with com.saabgroup.cms.pha.sensor.depth.Depth_Seq; use com.saabgroup.cms.pha.sensor.depth.Depth_Seq;
with com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport;

package body com.saabgroup.cms.pha.sensor.depth.Depth_SubscriberListener is
   use DDS;
   -----------------------
   -- On_Data_Available --
   -----------------------

   procedure On_Data_Available
     (Self       : not null access My_Listener;
      The_Reader : access DDS.DataReader.Ref'Class) is
      pragma Unreferenced (Self);

      Reader   : com.saabgroup.cms.pha.sensor.depth.Depth_DataReader.Ref_Access;
      Data_Seq : aliased Depth_Seq.Sequence;
      Info_Seq : aliased DDS.SampleInfo_Seq.Sequence;

   begin
      Reader :=  Depth_DataReader.Ref_Access (The_Reader);

      Reader.Take (Data_Seq'Access,
                   Info_Seq'Access,
                   DDS.LENGTH_UNLIMITED,
                   DDS.ANY_SAMPLE_STATE,
                   DDS.ANY_VIEW_STATE,
                   DDS.ANY_INSTANCE_STATE);


      for I in 1 .. Get_Length (Data_Seq'Access) loop
         if SampleInfo_Seq.Get_Reference (Info_Seq'Access, DDS.Natural (I)).Valid_Data then
            --  Ada.Text_IO.Put_Line (DDS.To_Standard_String (Get_Reference (Data_Seq'Access, I).msg));
            pragma Compile_Time_Warning (True, "Complete handling of message.");
            com.saabgroup.cms.pha.sensor.depth.Depth_TypeSupport.Print_Data (Get_Reference (Data_Seq'Access, I));
         end if;
      end loop;
      Reader.Return_Loan (Data_Seq'Access, Info_Seq'Access);
   end On_Data_Available;

end com.saabgroup.cms.pha.sensor.depth.Depth_SubscriberListener;

