with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Log is
   ANSI_Reset : constant String := ESC & "[0m";
   ANSI_Bold_Red : constant String := ESC & "[31;1m";

   procedure Error (Msg : String)
   is
      Formatted : constant String := ANSI_Bold_Red & Msg & ANSI_Reset;
   begin
      Put_Line (Standard_Error, Formatted);
   end Error;
end Log;
