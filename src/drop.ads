with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Libc_Interop;
use Libc_Interop;

package Drop is
   Bad_Perms : exception;

   type Target is record
      Uid : uid_t;
      Gid : gid_t;
      Shell_Path : Unbounded_String;
   end record;

   function Find_Target (Name : String) return Target;
   procedure Drop (To : Target);
private
   procedure Set_Uid_Safe (Uid : uid_t);

   procedure Set_Gid_Safe (Gid : gid_t);
end Drop;
