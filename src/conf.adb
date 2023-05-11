with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

package body Conf is
   type Error_Type is
      (No_Conf, No_Options, Bad_Opt, Bad_Verb, Bad_Target, Early_End,
      Expected_As);

   type Error (Err : Error_Type := No_Conf) is record
      Line : Natural;
      case Err is
         when Early_End => null;
         when No_Conf => null;
         when No_Options => null;
         when Bad_Opt => Opt : Unbounded_String;
         when Bad_Target => Target : Unbounded_String;
         when Bad_Verb => Verb : Unbounded_String;
         when Expected_As => Got : Unbounded_String;
      end case;
   end record;

   package Errors is new
      Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Error);

   package Rules is new
      Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Rule);

   Reported_Errors : Errors.Vector;
   Reported_Rules : Rules.Vector;

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
               Put_Line
                  ("No configuration file found. Does /etc/adage.conf exist?");

            when No_Options =>
               Put_Line
                  ("Options should be preceded by `:`.");

            when Bad_Opt =>
               Put_Line ("Unrecognized option: " & To_String (Err.Opt) & ".");

            when Bad_Verb =>
               Put_Line
                  ("Saw " & To_String (Err.Verb) & ": Must be permit/reject.");

            when Bad_Target =>
               Put_Line
                  ("Saw " &
                   To_String (Err.Target) & ": Must be u!user or g!group.");

            when Early_End => Put_Line ("Premature line end.");

            when Expected_As =>
               Put_Line ("Expected 'as', got: " & To_String (Err.Got));
         end case;
      end loop;
   end Log_Errors;

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

   function Parse_Rule_Effect
      (Token : Unbounded_String; Effect : out Rule_Effect) return Boolean
   is begin
      if Token = "permit" then
         Effect := Permit;
      elsif Token = "reject" then
         Effect := Reject;
      else
         return False;
      end if;

      return True;
   end Parse_Rule_Effect;

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

   procedure Parse (Line : String; Line_Number : Natural) is
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

      if Start >= Line'Last then
         Report ((Err => Early_End, Line => Line_Number));
         return;
      end if;

      if not Parse_Rule_Effect (Token, Effect) then
         Report ((Err => Bad_Verb, Verb => Token, Line => Line_Number));
         return;
      end if;

      Next_Token (Line, Token, Start);

      if Start >= Line'Last then
         Report ((Err => Early_End, Line => Line_Number));
         return;
      end if;

      if not Parse_Target (Token, Target_Actor, Target_Category) then
         Report ((Err => Bad_Target, Line => Line_Number, Target => Token));
         return;
      end if;

      Next_Token (Line, Token, Start);

      if Start >= Line'Last then
         Report ((Err => Early_End, Line => Line_Number));
         return;
      end if;

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

               if Option = "nopass" then
                  My_Rule.Opts.No_Pass := True;
               elsif Option = "keepenv" then
                  My_Rule.Opts.Keep_Env := True;
               else
                  Report ((Err => Bad_Opt, Line => Line_Number,
                     Opt => Option));
               end if;
            end loop;
         end if;
      end if;

      Rules.Append (Reported_Rules, My_Rule);
   end Parse;

   function Read_Rules return Boolean is
      C : File_Type;
      Line_Number : Natural := 1;
   begin
      Open (C, In_File, "/etc/adage.conf");

      while not End_Of_File (C) loop
         declare
            L : constant String := Get_Line (C);
         begin
            Parse (L, Line_Number);
            Line_Number := Line_Number + 1;
         end;
      end loop;

      return Reported_Errors.Is_Empty;
   exception
      when Name_Error =>
         Report ((Err => No_Conf, Line => 0));
         return False;
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
      (Drop_Target : String; Actor_Name : String; Actor_Groups : Groups.Vector)
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
      (Drop_Target : String; Actor_Name : String; Actor_Groups : Groups.Vector)
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

   function Is_Permitted
      (Drop_Target : String; Actor_Name : String; Actor_Groups : Groups.Vector)
      return Ticket
   is
      Allowed : Ticket :=
         Rules_Permit (Drop_Target, Actor_Name, Actor_Groups);
      Denied : constant Boolean :=
         Rules_Deny (Drop_Target, Actor_Name, Actor_Groups);
   begin
      Allowed.Permit := Allowed.Permit and then not Denied;
      return Allowed;
   end Is_Permitted;
end Conf;
