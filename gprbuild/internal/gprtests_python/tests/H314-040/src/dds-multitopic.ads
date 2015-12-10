
pragma Ada_05;
with DDS.TopicDescription;

package DDS.MultiTopic is

   type Ref is interface and DDS.TopicDescription.Ref;
   type Ref_Access is access all Ref'Class;

   function get_subscription_expression
     (Self : not null access Ref)
     return DDS.String is abstract;


--     function get_expression_parameters
--       (Self : not null access Ref)
--       return DDS.StringSeq;
--
--
--     function set_expression_parameters
--       (Self : not null access Ref;
--        expression_parameters : in DDS.StringSeq)
--       return DDS.ReturnCode_t;


end DDS.MultiTopic;
