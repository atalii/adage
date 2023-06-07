with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Cli;
with Conf;
with Drop;
with Exec;
with Ident;
with Log;
with Pass;

procedure Adg is
   type Action is (Help, Verify, Good_Conf, Bad_Conf);

   function Verify_Env return Action is
   begin
      case Cli.Init_Env is
         when Cli.Help =>
            return Help;
         when Cli.Verify =>
            return Verify;
         when Cli.Parse_Ok =>
            if Conf.Read_Rules then
               return Good_Conf;
            else
               Conf.Log_Errors;
               return Bad_Conf;
            end if;
      end case;
   end Verify_Env;

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
      when E : Drop.Bad_Perms =>
         Log.Error (Exception_Message (E));
      when E : Drop.No_Such_User =>
         Log.Error (Exception_Message (E));
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
   case Verify_Env is
      when Help => Help;
      when Verify => Verify;
      when Good_Conf => Run;
      when Bad_Conf => Set_Exit_Status (1);
   end case;
end Adg;
