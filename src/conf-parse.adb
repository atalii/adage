package body Conf.Parse
   with SPARK_Mode => On
is

   -----------------------
   -- Parse_Rule_Effect --
   -----------------------

   function Parse_Rule_Effect (Token : String) return Parse_Rule_Effect_T.R
   is
      R : Parse_Rule_Effect_T.R;
   begin
      if Token = "permit" then
         R := (Okay => True, V => Permit);
      elsif Token = "reject" then
         R := (Okay => True, V => Reject);
      else
         R := (Okay => False);
      end if;

      return R;
   end Parse_Rule_Effect;

   -------------------
   -- Consume_Token --
   -------------------

   function Consume_Token (Line : Strings.Bounded_String) return Lex_Result
   is
      use Strings;

      Tok_Start : Natural := 1;
      Tok_Last : Natural;
      Next_Start : Natural;
      T : Strings.Bounded_String;
      R : Strings.Bounded_String;
   begin
      for I in 1 .. Length (Line) loop
         Tok_Start := I;
         exit when Element (Line, I) /= ' ';
      end loop;

      Tok_Last := Tok_Start;

      for I in Tok_Start .. Length (Line) loop
         exit when Element (Line, I) = ' ';
         Tok_Last := I;
      end loop;

      T := Bounded_Slice (Line, Tok_Start, Tok_Last);

      Next_Start := Tok_Last;
      for I in Next_Start .. Length (Line) loop
         Next_Start := I;
         exit when Element (Line, I) /= ' ';
      end loop;

      Next_Start := Next_Start + 1;

      if Next_Start > Length (Line) then
         R := To_Bounded_String ("");
      else
         R := Bounded_Slice (Line, Next_Start, Length (Line));
      end if;

      return (Token => T, Rest => R);
   end Consume_Token;

end Conf.Parse;
