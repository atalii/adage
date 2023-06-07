with Interfaces.C.Strings;
use Interfaces.C.Strings;

with System;
with System.Address_To_Access_Conversions;

package body Drop is
   package Ptr_Handle is
      new System.Address_To_Access_Conversions (Object => Character);

   use Ptr_Handle;

   function Ensure
      (Addr : System.Address) return System.Address
   is begin
      if Ptr_Handle.To_Pointer (Addr) = null then
         raise No_Such_User;
      else
         return Addr;
      end if;
   end Ensure;

   function Find_Target (Name : String) return Target
   is
      C_String : chars_ptr := New_String (Name);

      Pw_Ent_Addr : constant System.Address :=
         Ensure (Get_Pw_Nam (C_String));

      Pw_Ent : constant Passwd_Pointer.Object_Pointer :=
         Passwd_Pointer.To_Pointer (Pw_Ent_Addr);
   begin
      Free (C_String);
      return
         (Uid => Pw_Ent.all.pw_uid,
          Gid => Pw_Ent.all.pw_gid,
          Shell_Path =>
            To_Unbounded_String (Value (Pw_Ent.all.pw_shell)));
   end Find_Target;

   procedure Drop (To : Target) is
   begin
      Set_Gid_Safe (To.Gid);
      Set_Uid_Safe (To.Uid);
   end Drop;

   procedure Set_Uid_Safe (Uid : uid_t)
   is
      Attempt : constant Integer := Set_Uid (Uid);
   begin
      if Attempt /= 0 then
         raise Bad_Perms;
      end if;
   end Set_Uid_Safe;

   procedure Set_Gid_Safe (Gid : gid_t)
   is
      Attempt : constant Integer := Set_Gid (Gid);
   begin
      if Attempt /= 0 then
         raise Bad_Perms;
      end if;
   end Set_Gid_Safe;
end Drop;
