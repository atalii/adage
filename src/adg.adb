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
   procedure Help is
   begin
      Put_Line ("usage: adg [@user] [cmd...]");
   end Help;

   procedure Run is
      Name : constant String := Ident.Read_User;
      Groups : constant Ident.Groups.Vector := Ident.Read_Groups;
      Ticket : constant Conf.Ticket := Conf.Is_Permitted
         (Cli.Drop_Target, Name, Groups);

      Allowed : constant Boolean := Ticket.Permit;
   begin
      Set_Exit_Status (1);

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
            Exec.Exec (
               Cli.Child_Args (Target.Shell_Path));
         end;
      end if;
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
   end Run;

   procedure Verify
   is
      Status : constant Boolean := Conf.Read_Rules;
   begin
      Conf.Log_Errors;
      Set_Exit_Status (Boolean'Pos (not Status));
   end Verify;
begin
   case Cli.Init_Env is
      when Cli.Help => Help;
      when Cli.Verify => Verify;
      when Cli.Parse_Ok =>
         if Conf.Read_Rules then
            Run;
         else
            Conf.Log_Errors;
            Set_Exit_Status (1);
         end if;
   end case;
end Adg;
