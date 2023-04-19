with Ada.Command_Line; use Ada.Command_Line;

with Interfaces.C; use Interfaces.C;

package body Cli is
   type Child_Action (Shell : Boolean := False) is record
      case Shell is
         when False => Cmd_Start : Natural := 1;
         when True => null;
      end case;
   end record;

   type chars_ptr_array_ptr is access all chars_ptr_array;

   Cli_Cmd : Child_Action;
   Cli_Target : Target_Str.Bounded_String
      := Target_Str.To_Bounded_String ("root");

   function Init_Target (Slug : String) return Boolean
   is
      Is_Target : constant Boolean := Slug (Slug'First) = '@';
   begin
      if Is_Target then
         Cli_Target :=
            Target_Str.To_Bounded_String (Slug (Slug'First + 1 .. Slug'Last));
      end if;

      return Is_Target;
   end Init_Target;

   function Init_Env return Boolean is
      Ac : constant Natural := Argument_Count;
      Offset : Natural := 0;
   begin
      if Ac = 1 and then Argument (1) = "--help" then
         return False;
      end if;

      if Ac = 0 then
         Cli_Cmd := (Shell => True);
         return True;
      end if;

      --  Look for an @target and advance the command offset if we find one.
      Offset := Offset + Boolean'Pos (Init_Target (Argument (1)));

      if Offset >= Ac then
         Cli_Cmd := (Shell => True);
      else
         Cli_Cmd := (Shell => False, Cmd_Start => 1 + Offset);
      end if;

      return True;
   end Init_Env;

   function Cmd_Offset return Natural is
   begin
      case Cli_Cmd.Shell is
         when True => return 0;
         when False => return Cli_Cmd.Cmd_Start;
      end case;
   end Cmd_Offset;

   function Drop_Target return String is
   begin
      return Target_Str.To_String (Cli_Target);
   end Drop_Target;

   function Child_Args (Default : Unbounded_String) return C_Compat_Args
   is
      Binary_Name : chars_ptr := Null_Ptr;
      Ac : constant Natural := Argument_Count;
      Argv_Length : constant size_t := size_t (Ac) + 1;
      Argv : constant chars_ptr_array_ptr
         := new chars_ptr_array (0 .. Argv_Length);
   begin
      case Cli_Cmd.Shell is
         when True =>
            Binary_Name := New_String (To_String (Default));
            Argv (0) := Binary_Name;
            Argv (1) := Null_Ptr;

         when False =>
            Binary_Name := New_String (Argument (Cli_Cmd.Cmd_Start));

            Argv (size_t (Ac - Cli_Cmd.Cmd_Start)) := Null_Ptr;

            for I in Cli_Cmd.Cmd_Start .. Ac loop
               declare
                  Source : constant String := Argument (I);
               begin
                  Argv (size_t (I - Cli_Cmd.Cmd_Start)) := New_String (Source);
               end;
            end loop;
      end case;

      return
         (Binary_Name => Binary_Name,
          Argv => Argv.all'Address);
   end Child_Args;
end Cli;
