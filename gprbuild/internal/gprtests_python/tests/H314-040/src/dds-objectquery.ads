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

with DDS.Object;

package DDS.ObjectQuery is

   type Ref is new DDS.Object.Ref with null record;

   expression_Repository_Id : constant Standard.String
     := "IDL:DDS/ObjectQuery/expression:1.0";

   function Get_expression
     (Self : in Ref)
     return DDS.String;

   parameters_Repository_Id : constant Standard.String
     := "IDL:DDS/ObjectQuery/parameters:1.0";

--     function Get_parameters
--       (Self : in Ref)
--       return DDS.StringSeq;
--
--     function set_query
--       (Self : in Ref;
--        expression : in DDS.String;
--        parameters : in DDS.StringSeq)
--       return DDS.Boolean;
--
--     set_query_Repository_Id : constant Standard.String
--       := "IDL:DDS/ObjectQuery/set_query:1.0";
--
--     function set_parameters
--       (Self : in Ref;
--        parameters : in DDS.StringSeq)
--       return DDS.Boolean;

   set_parameters_Repository_Id : constant Standard.String
     := "IDL:DDS/ObjectQuery/set_parameters:1.0";

   Repository_Id : constant Standard.String
     := "IDL:DDS/ObjectQuery:1.0";

end DDS.ObjectQuery;
