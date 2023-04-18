with Interfaces.C.Strings;
use Interfaces.C.Strings;

with System;

with Ada.Text_IO;
use Ada.Text_IO;

package body Exec is
   function Exec_Vp
      (File : chars_ptr; Argv : System.Address) return Integer;

   function Get_Errno return Integer;

   pragma Import
      (Convention => C, Entity => Exec_Vp, External_Name => "execvp");

   pragma Import
      (Convention => C, Entity => Get_Errno, External_Name => "get_errno");

   procedure Exec (Args : Cli.C_Compat_Args) is
      Status : constant Integer
         := Exec_Vp (Args.Binary_Name, Args.Argv);
   begin
      if Status < 0 then
         Put_Line ("Exec failed with errno: " & Get_Errno'Image);
      end if;
   end Exec;
end Exec;
