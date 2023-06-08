with Interfaces.C.Strings;
use Interfaces.C.Strings;

with System;
with System.Address_To_Access_Conversions;

--  Contains some glue to get into libc. The package contains a mix of safe
--  wrappers and unsafe library stubs; be careful.
package Libc_Interop is
   type uid_t is new Integer;
   type gid_t is new Integer;

   type Group_Array is array (Integer range <>) of gid_t;

   type Struct_Passwd is record
      pw_name : chars_ptr;
      pw_passwd : chars_ptr;
      pw_uid : uid_t;
      pw_gid : gid_t;
      gw_gecos : chars_ptr;
      pw_dir : chars_ptr;
      pw_shell : chars_ptr;
   end record
      with Convention => C;

   type Struct_Group is record
      gr_name : chars_ptr;
      gr_passwd : chars_ptr;
      gr_gid : gid_t;
      gr_mem : System.Address;
   end record
      with Convention => C;

   package Passwd_Pointer is new
      System.Address_To_Access_Conversions (Struct_Passwd);

   package Group_Pointer is new
      System.Address_To_Access_Conversions (Struct_Group);

   --  Stub for execvpe(3).
   function Exec_Vpe
      (File : chars_ptr; Argv : System.Address; Envp : System.Address)
      return Integer;

   --  Stub for getpwuid(3). Returns the address of a Struct_Passwd. Be careful
   --  with this; subsequent calls to any of the Get_Pw/Get_Gr family of
   --  functions can overwrite the contents. Don't free the returned address,
   --  it's static.
   function Get_Pw_Uid (uid : uid_t) return System.Address;

   --  Stub for getpwnam(3). See the warnings for Get_Pw_Uid.
   function Get_Pw_Nam (nam : chars_ptr) return System.Address;

   --  Stub for getgrnam(3). See the warnings for Get_Pw_Uid.
   function Get_Gr_Gid (gid : gid_t) return System.Address;

   --  Stub for getuid(2).
   function Get_Uid return uid_t;

   --  Stub for getegid(2).
   function Get_Egid return gid_t;

   --  Stub for getgroups(2).
   function Get_Groups (size : Integer; gids : System.Address) return Integer;

   --  Stub for setuid(2).
   function Set_Uid (uid : uid_t) return Integer;

   --  Stub for setgid(2).
   function Set_Gid (gid : gid_t) return Integer;

   --  Count the number of groups the user is a member in. This isn't a libc
   --  stub, and can be used safely from one thread.
   function Count_Groups return Integer;

   pragma Import
      (Convention => C, Entity => Exec_Vpe, External_Name => "execvpe");

   pragma Import
      (Convention => C, Entity => Get_Pw_Uid, External_Name => "getpwuid");

   pragma Import
      (Convention => C, Entity => Get_Pw_Nam, External_Name => "getpwnam");

   pragma Import
      (Convention => C, Entity => Get_Gr_Gid, External_Name => "getgrgid");

   pragma Import
      (Convention => C, Entity => Get_Uid, External_Name => "getuid");

   pragma Import
      (Convention => C, Entity => Get_Egid, External_Name => "getegid");

   pragma Import
      (Convention => C, Entity => Get_Groups, External_Name => "getgroups");

   pragma Import
      (Convention => C, Entity => Set_Uid, External_Name => "setuid");

   pragma Import
      (Convention => C, Entity => Set_Gid, External_Name => "setgid");
end Libc_Interop;
