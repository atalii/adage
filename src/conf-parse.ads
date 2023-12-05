with Ada.Strings.Bounded;

with Conf.Common; use Conf.Common;

package Conf.Parse
   with SPARK_Mode => On
is
   package Strings is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max => 1024);

   type Lex_Result is record
      Token : Strings.Bounded_String;
      Rest : Strings.Bounded_String;
   end record;

   function Consume_Token (Line : Strings.Bounded_String) return Lex_Result
      with Pre => Strings.Length (Line) > 0;
   --  Consume a token from a line, returning a Lex_Result. Result.Token is the
   --  first token encountered, and Rest is the string without that token at
   --  the head. Consume_Token may be called again on Result'Rest. A token is a
   --  string delimited by (potentially zero) spaces, roughly:
   --
   --  Token ::= " "* [^" "] " "*
   --
   --  This function will not interpret all whitespace as whitespace. Should
   --  make this UTF-8 aware.

   generic
      type Val is private;
   package Parse_Result is
      type R (Okay : Boolean := True) is record
         case Okay is
            when True => V : Val;
            when False => null;
         end case;
      end record;

      function Contains (This : R; V : Val) return Boolean
         is (This.Okay and then V = This.V);
   end Parse_Result;

   package Parse_Rule_Effect_T is new Parse_Result (Val => Rule_Effect);

   function Parse_Rule_Effect (Token : String) return Parse_Rule_Effect_T.R
      with Post =>
         ((Token = "permit") =
            (Parse_Rule_Effect_T.Contains (Parse_Rule_Effect'Result, Permit))
         and then (Token = "reject") =
            (Parse_Rule_Effect_T.Contains (Parse_Rule_Effect'Result, Reject))
         and then (Token = "permit" or else Token = "reject") =
            Parse_Rule_Effect'Result.Okay);
end Conf.Parse;
