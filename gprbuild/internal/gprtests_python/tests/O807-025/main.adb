with GPR; use GPR;
with GPR.Tree; use GPR.Tree;
with GPR.PP; use GPR.PP;
with GPR.Snames; use GPR.Snames;
with GPR.Env; use GPR.Env;
with GPR.Conf; use GPR.Conf;

with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Main is
   Root_Environment : GPR.Tree.Environment;
   Project_Node_Tree : constant GPR.Tree.Project_Node_Tree_Ref :=
     new GPR.Tree.Project_Node_Tree_Data;
   Project_Tree : constant GPR.Project_Tree_Ref :=
     new GPR.Project_Tree_Data (Is_Root_Tree => True);

   Main_Project : Project_Id;
   User_Project_Node : Project_Node_Id;
   Automatically_Generated : Boolean;
   Config_File_Path        : String_Access;
begin
   GPR.Snames.Initialize;
   GPR.Tree.Initialize (Root_Environment, Gprbuild_Flags);
   GPR.Tree.Initialize (Project_Node_Tree);
   GPR.Initialize (Project_Tree);
   GPR.Env.Initialize_Default_Project_Path
     (Root_Environment.Project_Path, Target_Name => "-");

   Main_Project := No_Project;

   Parse_Project_And_Apply_Config
     (Main_Project               => Main_Project,
      User_Project_Node          => User_Project_Node,
      Autoconf_Specified         => False,
      Project_File_Name          => "other.gpr",
      Project_Tree               => Project_Tree,
      Env                        => Root_Environment,
      Project_Node_Tree          => Project_Node_Tree,
      Packages_To_Check          => null,
      Automatically_Generated    => Automatically_Generated,
      Config_File_Path           => Config_File_Path,
      Normalized_Hostname        => "");

   Pretty_Print
     (User_Project_Node,
      Project_Node_Tree,
      Backward_Compatibility => False);

end Main;
