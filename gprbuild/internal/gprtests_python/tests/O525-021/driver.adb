with GPR;
with GPR.Env;
with GPR.Tree;
with GPR.Conf;
with GPR.Names;
with GPR.Snames;
with GPR.Opt;
with GPR.Proc;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Util; use Util;

procedure Driver is
   Root_Environment  : GPR.Tree.Environment;
   Project_Tree      : constant GPR.Project_Tree_Ref :=
                         new GPR.Project_Tree_Data (Is_Root_Tree => True);
   Project_Node_Tree : constant GPR.Tree.Project_Node_Tree_Ref :=
                         new GPR.Tree.Project_Node_Tree_Data;

   Config_Project_File_Name : String_Access := new String'("");
   Main_Project : GPR.Project_Id := GPR.No_Project;
   User_Project_Node : GPR.Project_Node_Id;

   Automatically_Generated : Boolean;
   Config_File_Path        : String_Access;

begin
   GPR.Opt.Setup_Projects := True;
   GPR.Opt.Quiet_Output := True;
   GPR.Snames.Initialize;

   GPR.Tree.Initialize (Root_Environment, GPR.Gprbuild_Flags);
   GPR.Tree.Initialize (Project_Node_Tree);

   GPR.Initialize (Project_Tree);

   GPR.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name => "-");

   GPR.Conf.Set_Runtime_For (GPR.Snames.Name_Ada, "sjlj");
   GPR.Proc.Set_Default_Runtime_For (GPR.Snames.Name_Ada, "sjlj");
   GPR.Conf.Parse_Project_And_Apply_Config
        (Main_Project               => Main_Project,
         User_Project_Node          => User_Project_Node,
         Config_File_Name           => Config_Project_File_Name.all,
         Autoconf_Specified         => False,
         Project_File_Name          => "prj.gpr",
         Project_Tree               => Project_Tree,
         Env                        => Root_Environment,
         Project_Node_Tree          => Project_Node_Tree,
         Packages_To_Check          => Packages_To_Check,
         Allow_Automatic_Generation => True,
         Automatically_Generated    => Automatically_Generated,
         Config_File_Path           => Config_File_Path,
         Target_Name                => "x86_64-linux",
         Normalized_Hostname        => "x86_64-linux",
         Implicit_Project           => False);
   declare
      Obj_Dir : constant String := 
                Gpr.Names.Get_Name_String (Main_Project.Object_Directory.Name);
   begin
      Put_Line (Obj_Dir (Obj_Dir'Last - 4 .. Obj_Dir'Last - 1));
   end;
end Driver;

