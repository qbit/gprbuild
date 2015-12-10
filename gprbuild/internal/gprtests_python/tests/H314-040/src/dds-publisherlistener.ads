pragma Ada_05;
-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with DDS.DataWriterListener;

package DDS.PublisherListener is

   type Ref is limited interface and DDS.DataWriterListener.Ref;
   type Ref_Access is access all Ref'Class;

end DDS.PublisherListener;
