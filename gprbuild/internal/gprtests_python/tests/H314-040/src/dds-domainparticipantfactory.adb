pragma Ada_05;

with System; use System;

with DDS.DomainParticipantFactory.ThreadListners;
use DDS.DomainParticipantFactory.ThreadListners;

with Ada.text_io;

with DDS.DomainParticipant_Impl; use DDS.DomainParticipant_Impl;

package body DDS.DomainParticipantFactory is

   use type DDS.DomainParticipantListener.Ref_Access;
   TheParticipantFactory : Ref_Access;
   Tasking_IF : aliased Thread_Listener_I;

   function C_Get_Default_P_Qos
     (Self    : in System.Address;
      Qos     : in System.Address) return DDS.ReturnCode_T;

   pragma Import (C, C_Get_Default_P_Qos,
                  "DDS_DomainParticipantFactory_get_default_participant_qos");

   function DDS_DomainParticipantFactory_get_instance return System.Address;
   pragma Import (C, DDS_DomainParticipantFactory_Get_Instance,
                  "DDS_DomainParticipantFactory_get_instance");

   procedure Initialize is
   begin
      TheParticipantFactory := TheParticipantFactoryImpl'Access;
      TheParticipantFactory.SetInterface (DDS_DomainParticipantFactory_Get_Instance);
      Ret_Code_To_Exception
         (C_Get_Default_P_Qos (DDS_DomainParticipantFactory_Get_Instance,
                               PARTICIPANT_QOS_DEFAULT.GetInterface));
      Set_Thread_ListenerI (DDS_DomainParticipantFactory_get_instance, Tasking_IF'Access);
   end Initialize;

   function Get_Instance return Ref_Access is
   begin
      if TheParticipantFactory = null then
         Initialize;
      end if;
      return TheParticipantFactory;
   end Get_Instance;

   ------------------------
   -- Create_Participant --
   ------------------------

   function Create_Participant
     (Self       : not null access Ref;
      Domain_Id  : in DDS.DomainId_T;
      Qos        : in DDS.DomainParticipantQos;
      A_Listener : in DDS.DomainParticipantListener.Ref_Access;
      Mask       : in DDS.StatusMask)
     return DDS.DomainParticipant.Ref_Access is
   begin
      return (DDS.DomainParticipant_Impl.CreateI (Self.GetInterface,
                                                  Domain_Id,
                                                  Qos,
                                                  A_Listener,
                                                  Mask));

   end Create_Participant;

   ------------------------
   -- Delete_Participant --
   ------------------------

   procedure Delete_Participant
     (Self          : not null access Ref;
      A_Participant : in out DDS.DomainParticipant.Ref_Access)
   is
      function Internal (Self                 : System.Address;
                         A_Participant        : System.Address)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipantFactory_delete_participant");

      P : DDS.DomainParticipant_Impl.Ref_Access :=
            DDS.DomainParticipant_Impl.Ref_Access (A_Participant);

   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, P.GetInterface),
                             "Unable to delete participant");
      DomainParticipant_Impl.Free (P);
   end Delete_Participant;

   ------------------------
   -- Lookup_Participant --
   ------------------------

   function Lookup_Participant
     (Self      : not null access Ref;
      Domain_Id : in DDS.DomainId_T)
      return DDS.DomainParticipant.Ref_Access
   is
   begin
      raise Not_Implemented;
      return null;
   end Lookup_Participant;

   ---------------------------------
   -- Set_Default_Participant_Qos --
   ---------------------------------

   procedure Set_Default_Participant_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantQos)
   is
      function Internal
        (Self    : in System.Address;
         Qos     : in System.Address)
         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipantFactory_set_default_participant_qos");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos.GetInterface));

   end Set_Default_Participant_Qos;

   ---------------------------------
   -- Get_Default_Participant_Qos --
   ---------------------------------

   procedure Get_Default_Participant_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.DomainParticipantQos)
   is
   begin
      Ret_Code_To_Exception
        (C_Get_Default_P_Qos (Self.GetInterface, Qos.GetInterface));

   end Get_Default_Participant_Qos;

   -------------
   -- Set_Qos --
   -------------

   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DomainParticipantFactoryQos) is
      function Internal
        (Self    : in System.Address;
         Qos     : in System.Address) return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipantFactory_set_qos");

   begin
      Ret_Code_To_Exception
        (Internal (Self.GetInterface, Qos'Address));
   end Set_Qos;

   -------------
   -- Get_Qos --
   -------------
   procedure Get_Qos
     (Self    : not null access Ref;
      Qos     : in out DDS.DomainParticipantFactoryQos) is
      function Internal
        (Self    : in System.Address;
         Qos     : in System.Address) return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipantFactory_get_qos");

   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface, Qos'Address));
   end Get_Qos;


   procedure Finalize_Instance (Self    : not null access Ref) is
      function Internal (Self                 : System.Address)
                         return DDS.ReturnCode_T;
      pragma Import (C, Internal, "DDS_DomainParticipantFactory_finalize_instance");
   begin
      Ret_Code_To_Exception (Internal (Self.GetInterface));
      TheParticipantFactory.SetInterface (System.Null_Address);
      TheParticipantFactory := null;
   end Finalize_Instance;


begin
   Initialize;
end DDS.DomainParticipantFactory;
