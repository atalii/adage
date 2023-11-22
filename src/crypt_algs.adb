package body Crypt_Algs
   with SPARK_Mode => On
is
   function Constant_Time_Compare (A, B : String) return Boolean
      with SPARK_Mode => Off
   is begin
      if
         A'Length /= B'Length or else
         A'Length = 0
      then
         raise Invalid_Comparison;
      end if;

      return Constant_Time_Compare_Inner (A, B);
   end Constant_Time_Compare;

   function Constant_Time_Compare_Inner (A, B : String) return Boolean
   is
      R : Boolean := True;
      A_I : Character;
      B_I : Character;
   begin
      for I in 0 .. (A'Length - 1) loop
         A_I := A (A'First + I);
         B_I := B (B'First + I);
         R := R and (A_I = B_I);

         pragma Loop_Invariant
            (R = (for all J in A'First .. (A'First + I) =>
               A (J) = B (B'First - A'First + J)));
      end loop;

      return R;
   end Constant_Time_Compare_Inner;
end Crypt_Algs;
