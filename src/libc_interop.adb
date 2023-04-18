package body Libc_Interop is
   function Count_Groups return Integer is
   begin
      return Get_Groups (0, System.Null_Address);
   end Count_Groups;
end Libc_Interop;
