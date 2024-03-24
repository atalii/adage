with Interfaces.C.Strings;
use Interfaces.C.Strings;

package Get_Errno_Pkg is
   function Get_Errno return Integer;

   function Describe return String;

   EAGAIN : Integer;
   EINVAL : Integer;
   ENOENT : Integer;
   EPERM : Integer;

   pragma Import
      (Convention => C, Entity => Get_Errno, External_Name => "get_errno");

   pragma Import
      (Convention => C, Entity => EAGAIN, External_Name => "ERRNO_EAGAIN");

   pragma Import
      (Convention => C, Entity => EINVAL, External_Name => "ERRNO_EINVAL");

   pragma Import
      (Convention => C, Entity => ENOENT, External_Name => "ERRNO_ENOENT");

   pragma Import
      (Convention => C, Entity => EPERM, External_Name => "ERRNO_EPERM");
private
   function Str_Error (Errnum : Integer) return chars_ptr;

   pragma Import
      (Convention => C, Entity => Str_Error, External_Name => "strerror");
end Get_Errno_Pkg;
