package Crypt_Algs
   with SPARK_Mode => On
is
   function Constant_Time_Compare (A, B : String) return Boolean;

   Invalid_Comparison : exception;
private
   function Constant_Time_Compare_Inner (A, B : String) return Boolean
   with
      Pre =>
         (A'Length = B'Length and then A'Length > 0),
      Post => (Constant_Time_Compare_Inner'Result = (A = B));
end Crypt_Algs;
