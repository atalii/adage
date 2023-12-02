package body Conf.Parse
   with SPARK_Mode => On
is
   -----------------------
   -- Parse_Rule_Effect --
   -----------------------

   function Parse_Rule_Effect (Token : String) return Parse_Rule_Effect_T.R
   is begin
      if Token = "permit" then
         return (Okay => True, V => Permit);
      elsif Token = "reject" then
         return (Okay => True, V => Reject);
      else
         return (Okay => False);
      end if;
   end Parse_Rule_Effect;
end Conf.Parse;
