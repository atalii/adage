with Ada.Strings.Bounded;

with Conf.Common; use Conf.Common;

package Conf.Parse
   with SPARK_Mode => On
is
   package Strings is new
      Ada.Strings.Bounded.Generic_Bounded_Length (Max => 1024);
   --  These strings are responsible for representing lines and portions of
   --  lines.

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

   function Consume_Token
      (Line : in out Strings.Bounded_String)
      return Strings.Bounded_String;
   --  Consume a token from Line and return it. Line itself is modified so as
   --  to no longer include the token at the head. A token is a string
   --  delimited by (potentially zero) spaces, roughly:
   --
   --  Token ::= " "* [^" "] " "*
   --
   --  This function will not interpret all whitespace as whitespace. Should
   --  make this UTF-8 aware.
end Conf.Parse;
