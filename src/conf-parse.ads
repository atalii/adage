with Conf.Common; use Conf.Common;

package Conf.Parse
   with SPARK_Mode => On
is
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
