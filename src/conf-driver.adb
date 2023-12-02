with Ada.Text_IO; use Ada.Text_IO;

with Log;

with Get_Errno_Pkg;
use Get_Errno_Pkg;

with Conf.Parse; use Conf.Parse;

package body Conf.Driver is
   type Error_Type is
      (No_Conf, Bad_Conf_Perms, No_Stat, No_Options, Bad_Opt, Bad_Verb,
      Bad_Target, Early_End, Expected_As);

   type Error (Err : Error_Type := No_Conf) is record
      Line : Natural;
      case Err is
         when Early_End => null;
         when No_Conf => null;
         when Bad_Conf_Perms => null;
         when No_Options => null;
         when No_Stat => Errno : Integer;
         when Bad_Opt => Opt : Unbounded_String;
         when Bad_Target => Target : Unbounded_String;
         when Bad_Verb => Verb : Unbounded_String;
         when Expected_As => Got : Unbounded_String;
      end case;
   end record;

   package Errors is new
      Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Error);

   Reported_Errors : Errors.Vector;

   procedure Report (Err : Error) is
   begin
      Errors.Append (Reported_Errors, Err);
   end Report;

   procedure Log_Errors is
   begin
      for Err of Reported_Errors loop
         Put ("[" & Err.Line'Image & "] ");

         case Err.Err is
            when No_Conf =>
               Log.Error
                  ("No configuration file found. Does /etc/adage.conf exist?");

            when Bad_Conf_Perms =>
               Log.Error
                  ("adage.conf must be root:root/0644 or similar.");

            when No_Stat =>
               Log.Error
                  ("Could not stat(2) adage.conf. Errno: " & Err.Errno'Image);

            when No_Options =>
               Log.Error
                  ("Options should be preceded by `:`.");

            when Bad_Opt =>
               Log.Error
                  ("Unrecognized option: " & To_String (Err.Opt) & ".");

            when Bad_Verb =>
               Log.Error
                  ("Saw " & To_String (Err.Verb) & ": Must be permit/reject.");

            when Bad_Target =>
               Log.Error
                  ("Saw " &
                   To_String (Err.Target) & ": Must be u!user or g!group.");

            when Early_End => Log.Error ("Premature line end.");

            when Expected_As =>
               Log.Error ("Expected 'as', got: " & To_String (Err.Got));
         end case;
      end loop;
   end Log_Errors;

   function Check_Eol
      (Start : Natural; Line : String; Line_Number : Integer)
      return Boolean
   is
      Eol : constant Boolean := Start >= Line'Length;
   begin
      if Eol then
         Report ((Err => Early_End, Line => Line_Number));
      end if;

      return Eol;
   end Check_Eol;

   procedure Next_Token
      (Line : String; Token : out Unbounded_String; Start : in out Natural)
   is
      Last : Natural;
      C : Character;
   begin
      if Start >= Line'Length then
         return;
      end if;

      for I in Start .. Line'Length loop
         C := Line (I);
         Start := I;
         exit when C /= ' ';
      end loop;

      Last := Start;

      for I in Start .. Line'Length loop
         C := Line (I);
         exit when C = ' ';
         Last := I;
      end loop;

      Token := To_Unbounded_String (Line (Start .. Last));

      for I in Last .. Line'Length loop
         C := Line (I);
         exit when C /= ' ';
         Last := I;
      end loop;
      Start := Last + 1;
   end Next_Token;

   function Parse_Target
      (Token : Unbounded_String;
       Target : out Unbounded_String;
       Target_Category : out Category) return Boolean
   is begin
      if Token'Size <= 3 then
         return False;
      end if;

      if Head (Token, 2) = "g!" then
         Target_Category := Group;
      elsif Head (Token, 2) = "u!" then
         Target_Category := User;
      else
         return False;
      end if;

      Target := Tail (Token, Length (Token) - 2);
      return True;
   end Parse_Target;

   procedure Parse_Option
      (Token : Unbounded_String;
       Opts : in out Options; Line_Number : Natural) is
   begin
      if Token = "nopass" then
         Opts.No_Pass := True;
      elsif Token = "keepenv" then
         Opts.Keep_Env := True;
      else
         Report ((Err => Bad_Opt, Line => Line_Number,
            Opt => Token));
      end if;
   end Parse_Option;

   -----------
   -- Parse --
   -----------

   procedure Parse (Line : String; Line_Number : Natural; R : out Rules) is
      Token : Unbounded_String;
      Start : Natural := Line'First;
      Effect : Rule_Effect;
      Target_Actor : Unbounded_String;
      Target_Category : Category;
      Drop_Actor : Unbounded_String;
      Option : Unbounded_String;
      My_Rule : Rule;
   begin
      if Line = "" or else Line (Line'First) = '#' then
         return;
      end if;

      Next_Token (Line, Token, Start);

      if Check_Eol (Start, Line, Line_Number) then
         return;
      end if;

      declare
         R : constant Parse_Rule_Effect_T.R :=
            Parse_Rule_Effect (To_String (Token));
      begin
         case R.Okay is
            when True => Effect := R.V;
            when False =>
               Report ((Err => Bad_Verb, Verb => Token, Line => Line_Number));
               return;
         end case;
      end;

      Next_Token (Line, Token, Start);

      if Check_Eol (Start, Line, Line_Number) then
         return;
      end if;

      if not Parse_Target (Token, Target_Actor, Target_Category) then
         Report ((Err => Bad_Target, Line => Line_Number, Target => Token));
         return;
      end if;

      Next_Token (Line, Token, Start);

      if Token /= "as" then
         Report ((Err => Expected_As, Got => Token, Line => Line_Number));
         return;
      end if;

      Next_Token (Line, Drop_Actor, Start);

      My_Rule :=
         (Effect => Effect,
          Target_Category => Target_Category,
          Target_Actor => Target_Actor,
          Drop_Actor => Drop_Actor,
          Opts => (No_Pass => False, Keep_Env => False));

      if Start < Line'Last then
         if Tail (Drop_Actor, 1) /= ":" then
            Report ((Err => No_Options, Line => Line_Number));
         else
            My_Rule.Drop_Actor := To_Unbounded_String
               (Slice (My_Rule.Drop_Actor, 1, Length (Drop_Actor) - 1));

            loop
               exit when Start >= Line'Last;
               Next_Token (Line, Option, Start);
               Parse_Option (Option, My_Rule.Opts, Line_Number);
            end loop;
         end if;
      end if;

      R.Vec.Append (My_Rule);
   end Parse;

   ----------------
   -- Read_Rules --
   ----------------

   function Read_Rules return Rules is
      C : File_Type;
      Line_Number : Natural := 1;
      R : constant Integer := Check_Conf_Perms;
      Errno : constant Integer := Get_Errno;

      Ret : Rules := (Vec => Rules_Vec.Empty_Vector);
   begin
      if R < 0 and then Errno = ENOENT then
         Report ((Err => No_Conf, Line => 0));
         raise Parse_Failure;
      elsif R < 0 then
         Report ((Err => No_Stat, Errno => Errno, Line => 0));
      elsif R = 0 then
         Report ((Err => Bad_Conf_Perms, Line => 0));
      end if;

      Open (C, In_File, "/etc/adage.conf");

      while not End_Of_File (C) loop
         declare
            L : constant String := Get_Line (C);
         begin
            Parse (L, Line_Number, Ret);
            Line_Number := Line_Number + 1;
         end;
      end loop;

      if not Reported_Errors.Is_Empty then
         raise Parse_Failure;
      end if;

      return Ret;
   exception
      when Name_Error =>
         Report ((Err => No_Conf, Line => 0));
         raise Parse_Failure;
   end Read_Rules;

   function Applicable
      (My_Rule : Rule;
       Drop_Target : String;
       Actor_Name : String;
       Actor_Groups : Groups.Vector)
      return Boolean is
   begin
      if My_Rule.Drop_Actor /= Drop_Target
         and then My_Rule.Drop_Actor /= "*"
      then
         return False;
      end if;

      if My_Rule.Target_Category = User then
         if My_Rule.Target_Actor /= Actor_Name then
            return False;
         end if;
      end if;

      if My_Rule.Target_Category = Group then
         if not Groups.Contains
            (Actor_Groups, To_String (My_Rule.Target_Actor))
         then
            return False;
         end if;
      end if;

      return True;
   end Applicable;

   function Rules_Permit
      (Reported_Rules : Rules_Vec.Vector;
       Drop_Target : String;
       Actor_Name : String;
       Actor_Groups : Groups.Vector)
      return Ticket
   is
      T : Ticket :=
         (Permit => False, Opts => (Keep_Env => False, No_Pass => False));
   begin
      for Rule of Reported_Rules loop
         if Rule.Effect = Permit and then Applicable
            (Rule, Drop_Target, Actor_Name, Actor_Groups)
         then
            T.Permit := True;
            T.Opts.Keep_Env := T.Opts.Keep_Env or else Rule.Opts.Keep_Env;
            T.Opts.No_Pass := T.Opts.No_Pass or else Rule.Opts.No_Pass;
         end if;
      end loop;

      return T;
   end Rules_Permit;

   function Rules_Deny
      (Reported_Rules : Rules_Vec.Vector;
       Drop_Target : String;
       Actor_Name : String;
       Actor_Groups : Groups.Vector)
      return Boolean is
   begin
      for Rule of Reported_Rules loop
         if Rule.Effect = Reject and then Applicable
            (Rule, Drop_Target, Actor_Name, Actor_Groups)
         then
            return True;
         end if;
      end loop;

      return False;
   end Rules_Deny;

   ------------------
   -- Is_Permitted --
   ------------------

   function Is_Permitted
      (R : Rules;
       Drop_Target : String;
       Actor_Name : String;
       Actor_Groups : Groups.Vector)
      return Ticket
   is
      Reported_Rules : constant Rules_Vec.Vector := R.Vec;

      Allowed : Ticket :=
         Rules_Permit (Reported_Rules, Drop_Target, Actor_Name, Actor_Groups);
      Denied : constant Boolean :=
         Rules_Deny (Reported_Rules, Drop_Target, Actor_Name, Actor_Groups);
   begin
      Allowed.Permit := Allowed.Permit and then not Denied;
      return Allowed;
   end Is_Permitted;
end Conf.Driver;
