with Ada.Text_IO; use Ada.Text_IO;

package body Pass is
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
