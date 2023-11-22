with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with Cli;
with Conf;
with Drop;
with Exec;
with Ident;
with Log;
with Pass;

procedure Adg is

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Put_Line ("usage: adg [@user] [cmd...]");
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Rules : Conf.Rules) is
      Name : constant String := Ident.Read_User;
      Groups : constant Ident.Groups.Vector := Ident.Read_Groups;
      Ticket : constant Conf.Ticket := Conf.Is_Permitted
         (Rules, Cli.Drop_Target, Name, Groups);

      Allowed : constant Boolean := Ticket.Permit;
   begin
      if not Allowed then
         Log.Error
            ("Rules do not allow for "
             & Name
             & " to authenticate as "
             & Cli.Drop_Target
             & ".");
      elsif not Ticket.Opts.No_Pass and then not Pass.Verify (Name) then
         Log.Error ("Password authentication failed.");
      else
         declare
            Target : constant Drop.Target :=
               Drop.Find_Target (Cli.Drop_Target);
         begin
            Drop.Drop (Target);
            Exec.Exec
               (Cli.Child_Args (Target.Shell_Path), Ticket.Opts.Keep_Env);
         end;
      end if;
   end Run;

   Rules : Conf.Rules;

begin

   Set_Exit_Status (1);

   case Cli.Init_Env is
      when Cli.Help =>
         Help;
         Set_Exit_Status (0);

      when Cli.Verify =>
         Rules := Conf.Read_Rules;
         Set_Exit_Status (0);

      when Cli.Parse_Ok =>
         Rules := Conf.Read_Rules;
         Run (Rules);
   end case;

exception

   when Drop.Bad_Perms =>
      Log.Error ("Failed to drop to user, permission denied: " &
       "Is the binary setuid?");

   when Drop.Bad_Id =>
      Log.Error ("Target uid or gid is invalid in this namespace.");

   when Drop.No_Such_User =>
      Log.Error ("No such user: " & Cli.Drop_Target);

   when Pass.Cannot_Read =>
      Log.Error ("Cannot read password. Exiting.");

   when Conf.Parse_Failure => Conf.Log_Errors;
end Adg;
