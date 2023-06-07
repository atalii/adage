with System;

with Interfaces.C.Strings;
use Interfaces.C.Strings;

with Ada.Strings.Bounded;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Cli is
   type C_Compat_Args is record
      Binary_Name : chars_ptr;
      Argv : System.Address;
   end record;

   type Parse_Result is (Help, Verify, Parse_Ok);

   --  Initialize package-global variables by reading the binary's command
   --  line. One may only call other functions if this returns Parse_Ok.
   function Init_Env return Parse_Result;

   function Cmd_Offset return Natural;
   function Drop_Target return String;

   --  Allocate children for a returned C_Compat_Args. This code is basically
   --  C, be careful.
   function Child_Args (Default : Unbounded_String) return C_Compat_Args;
private
   package Target_Str is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max => 128);
end Cli;
