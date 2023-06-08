with System;

with Get_Errno_Pkg;
use Get_Errno_Pkg;

with Libc_Interop;
use Libc_Interop;

with Log;

package body Exec is
   procedure Exec (Args : Cli.C_Compat_Args; Keep_Env : Boolean) is
      Env : constant System.Address :=
         (if Keep_Env then Environ else System.Null_Address);

      Status : constant Integer
         := Exec_Vpe (Args.Binary_Name, Args.Argv, Env);
      R : Integer;
   begin
      if Status < 0 then
         R := Get_Errno;
         if R = ENOENT then
            Log.Error ("Command not found.");
         else
            Log.Error ("Exec failed with errno: " & R'Image);
         end if;
      end if;
   end Exec;
end Exec;
