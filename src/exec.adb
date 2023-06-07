with Interfaces.C.Strings;
use Interfaces.C.Strings;

with System;

with Ada.Text_IO;
use Ada.Text_IO;

with Get_Errno_Pkg;
use Get_Errno_Pkg;

package body Exec is
   function Exec_Vp
      (File : chars_ptr; Argv : System.Address) return Integer;

   pragma Import
      (Convention => C, Entity => Exec_Vp, External_Name => "execvp");

   procedure Exec (Args : Cli.C_Compat_Args) is
      Status : constant Integer
         := Exec_Vp (Args.Binary_Name, Args.Argv);
      R : Integer;
   begin
      if Status < 0 then
         R := Get_Errno;
         if R = ENOENT then
            Put_Line ("Command not found.");
         else
            Put_Line ("Exec failed with errno: " & R'Image);
         end if;
      end if;
   end Exec;
end Exec;
