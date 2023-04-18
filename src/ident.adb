with Interfaces.C.Strings;
use Interfaces.C.Strings;

with System;

with Libc_Interop; use Libc_Interop;

package body Ident is
   function Read_User return String
   is
      Uid : constant uid_t := Get_Uid;
      Passwd_Addr : constant System.Address := Get_Pw_Uid (Uid);
      Passwd_Accs : constant Passwd_Pointer.Object_Pointer :=
         Passwd_Pointer.To_Pointer (Passwd_Addr);
   begin
      return Value (Passwd_Accs.all.pw_name);
   end Read_User;

   function Read_Groups return Groups.Vector
   is
      Size : constant Integer := Count_Groups;
      Buffer : Group_Array (1 .. Size);
      Buff_Ptr : constant System.Address := Buffer'Address;
      Read_Amount : constant Integer := Get_Groups (Size, Buff_Ptr);

      Egid : constant gid_t := Get_Egid;

      Names : Groups.Vector;

      Group_Entry_Ptr : System.Address;
      Group_Entry : Group_Pointer.Object_Pointer;
   begin
      if Read_Amount /= Size then
         raise Program_Error with "Could not fill group id array.";
      end if;

      for Gid of Buffer loop
         if Gid /= Egid then
            Group_Entry_Ptr := Get_Gr_Gid (Gid);
            Group_Entry := Group_Pointer.To_Pointer (Group_Entry_Ptr);
            Names.Append (Value (Group_Entry.gr_name));
         end if;
      end loop;

      return Names;
   end Read_Groups;
end Ident;
