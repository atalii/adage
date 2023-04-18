package Get_Errno_Pkg is
   function Get_Errno return Integer;

   pragma Import
      (Convention => C, Entity => Get_Errno, External_Name => "get_errno");
end Get_Errno_Pkg;
