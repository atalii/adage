with Ada.Text_IO;
use Ada.Text_IO;

with Interfaces;
use Interfaces;

package body Shadow is
   function Read_Hash (User : String) return String
   is
      Shadow_File : File_Type;
   begin
      Open (Shadow_File, In_File, "/etc/shadow");

      while not End_Of_File (Shadow_File) loop
         declare
            Line : constant String := Get_Line (Shadow_File);
         begin
            if Match_Line (Line, User) then
               return Get_Hash_Field (Line);
            end if;
         end;
      end loop;

      raise No_Entry;
   end Read_Hash;

   function Match (Hash : String; Key : String) return Boolean
   is
      Salt_Split : constant Natural := Get_Salt_Split (Hash) - 1;
      Salt : constant String := Hash (Hash'First .. Salt_Split);
      Hashed : constant String := Hash_Text (Key, Salt);
   begin
      return Constant_Time_Compare (Hash, Hashed);
   end Match;

   function Match_Line
      (Line : String; User : String) return Boolean
   is
      User_Len : constant Natural := User'Length;
      Slug : constant String := User & ":";
   begin
      if Line'Length < Line'First + User_Len then
         return False;
      end if;

      return
         Line (Line'First .. Line'First + User_Len) = Slug;
   end Match_Line;

   function Get_Salt_Split (Hash : String) return Natural
   is
      Count : Integer := 0;
   begin
      for I in Hash'First .. Hash'Last loop
         if Hash (I) = '$' then
            Count := Count + 1;
         end if;

         if Count = 3 then
            return I;
         end if;
      end loop;

      raise Invalid_Hash_Field;
   end Get_Salt_Split;

   function Get_Hash_Field (Line : String) return String
   is
      Start : Natural := Line'First;
      Last : Natural := Start;
      C : Character;
   begin
      for I in Start .. Line'Length loop
         C := Line (I);
         Start := I;
         exit when C = ':';
      end loop;

      Start := Start + 1;

      for I in Start .. Line'Length loop
         C := Line (I);
         exit when C = ':';
         Last := I;
      end loop;

      return Line (Start .. Last);
   end Get_Hash_Field;

   function Hash_Text (Key : String; Data : String) return String
   is
      Key_C : chars_ptr := New_String (Key);
      Data_C : chars_ptr := New_String (Data);
      Hashed_C : constant chars_ptr := Crypt (Key_C, Data_C);
      Hashed : constant String := Value (Hashed_C);
   begin
      Free (Key_C);
      Free (Data_C);
      return Hashed;
   end Hash_Text;

   function Constant_Time_Compare (A, B : String) return Boolean
   is
      pragma Optimize (Off);

      R : Unsigned_8 := 0;

      A_I : Character;
      B_I : Character;

      A_I_T : Unsigned_8;
      B_I_T : Unsigned_8;
   begin
      if A'Length /= B'Length then
         raise Invalid_Comparison;
      end if;

      for I in 0 .. (A'Length - 1) loop
         A_I := A (A'First + I);
         B_I := B (B'First + I);

         A_I_T := Standard.Character'Pos (A_I);
         B_I_T := Standard.Character'Pos (B_I);

         R := R or (A_I_T xor B_I_T);
      end loop;

      return R = 0;
   end Constant_Time_Compare;
end Shadow;
