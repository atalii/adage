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
end Conf.Parse;
