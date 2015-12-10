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

with DDS.Topic;
with DDS.TopicDescription;

package DDS.ContentFilteredTopic is

   type Ref is interface and DDS.TopicDescription.Ref;
   type Ref_Access is access all Ref'Class;

   function get_filter_expression
     (Self : not null access Ref)
     return DDS.String is abstract;

--     function get_expression_parameters
--       (Self : not null access Ref)
--       return DDS.StringSeq;
--
--     function set_expression_parameters
--       (Self : not null access Ref;
--        expression_parameters : in DDS.StringSeq)
--       return DDS.ReturnCode_t;

   function get_related_topic
     (Self : not null access Ref)
     return DDS.Topic.Ref_Access is abstract;

end DDS.ContentFilteredTopic;
