pragma Ada_05;
package body DDS.ObjectQuery is

   --------------------
   -- Get_expression --
   --------------------

   function Get_expression
     (Self : in Ref)
      return DDS.String
   is
   begin
      return Get_expression (Self);
   end Get_expression;

   --------------------
   -- Get_parameters --
   --------------------

--     function Get_parameters
--       (Self : in Ref)
--        return DDS.StringSeq
--     is
--     begin
--        return Get_parameters (Self);
--     end Get_parameters;
--
--     ---------------
--     -- set_query --
--     ---------------
--
--     function set_query
--       (Self : in Ref;
--        expression : in DDS.String;
--        parameters : in DDS.StringSeq)
--        return DDS.Boolean
--     is
--     begin
--        return set_query (Self, expression, parameters);
--     end set_query;
--
--     --------------------
--     -- set_parameters --
--     --------------------
--
--     function set_parameters
--       (Self : in Ref;
--        parameters : in DDS.StringSeq)
--        return DDS.Boolean
--     is
--     begin
--        return set_parameters (Self, parameters);
--     end set_parameters;

end DDS.ObjectQuery;
