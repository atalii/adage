with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Cli;
with Conf;
with Drop;
with Exec;
with Ident;
with Pass;

procedure Adg is
   function Verify_Env return Boolean
   is
      Status : constant Boolean := Cli.Init_Env and then Conf.Read_Rules;
   begin
      Conf.Log_Errors;
      return Status;
   end Verify_Env;

   procedure Help is
   begin
      Put_Line ("usage: adg [@user] [cmd...]");
   end Help;

   procedure Run is
      Name : constant String := Ident.Read_User;
      Groups : constant Ident.Groups.Vector := Ident.Read_Groups;
      Allowed : constant Boolean := Conf.Is_Permitted
         (Cli.Drop_Target,
          Name,
          Groups);
   begin
      if not Allowed then
         Put_Line
            ("No rule allows for "
             & Name
             & " to authenticate as "
             & Cli.Drop_Target
             & ".");
      elsif not Pass.Verify (Name) then
         Put_Line ("Password authentication failed.");
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
         Put_Line (Exception_Message (E));
      when E : Drop.No_Such_User =>
         Put_Line (Exception_Message (E));
   end Run;
begin
   if not Verify_Env then
      Help;
   else
      Run;
   end if;
end Adg;
