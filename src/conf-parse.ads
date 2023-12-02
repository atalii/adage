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
   end Parse_Result;

   package Parse_Rule_Effect_T is new Parse_Result (Val => Rule_Effect);

   function Parse_Rule_Effect (Token : String) return Parse_Rule_Effect_T.R;
end Conf.Parse;
