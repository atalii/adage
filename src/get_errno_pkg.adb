package body Get_Errno_Pkg is
   function Describe return String
   is
      Errno : constant Integer := Get_Errno;
      Raw : constant chars_ptr := Str_Error (Errno);
   begin
      return Value (Raw);
   end Describe;
end Get_Errno_Pkg;
