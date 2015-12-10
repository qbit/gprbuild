pragma Ada_05;

with DDS.DataReader;
with DDS.DataWriter;

package DDS.TypeSupport is

   type Ref is interface;
   type Ref_Access is access all Ref'Class;

   function Create_TypedDataReaderI (Self : access Ref) return DDS.DataReader.Ref_Access
      is abstract;

   procedure Destroy_TypedDataReaderI (Self   : access Ref;
                                      Reader : in out DDS.DataReader.Ref_Access)
      is abstract;

   function Create_TypedDataWriterI (Self : access Ref)
                                    return DDS.DataWriter.Ref_Access
      is abstract;

   procedure Destroy_TypedDataWriterI (Self   : access Ref;
                                      Writer : in out DDS.DataWriter.Ref_Access)
      is abstract;

end DDS.TypeSupport;
