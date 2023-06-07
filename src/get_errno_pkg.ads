package Get_Errno_Pkg is
   function Get_Errno return Integer;

   ENOENT : Integer;

   pragma Import
      (Convention => C, Entity => Get_Errno, External_Name => "get_errno");

   pragma Import
      (Convention => C, Entity => ENOENT, External_Name => "ERRNO_ENOENT");
end Get_Errno_Pkg;
