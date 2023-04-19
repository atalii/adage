with Ada.Text_IO; use Ada.Text_IO;

with Shadow;

package body Pass is
   function Verify (User : String) return Boolean is
      Entered : constant String := Read;
      Hashed : constant String := Shadow.Read_Hash (User);
   begin
      return Shadow.Match (Hashed, Entered);
   end Verify;

   function Read return String is
   begin
      if Term_Init < 0 then
         raise Not_A_TTY;
      end if;

      if Echo_Disable < 0 then
         raise Cannot_Modify_TTY;
      end if;

      Put ("Password: ");

      declare
         Input : constant String := Get_Line;
      begin
         if Echo_Enable < 0 then
            raise Cannot_Modify_TTY;
         end if;

         Put_Line ("");
         return Input;
      end;
   end Read;
end Pass;
