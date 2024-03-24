with Interfaces.C.Strings;
use Interfaces.C.Strings;
package Shadow is
   --  Read the hash field from /etc/passwd. Do not interpret it; instead
   --  treat it as a string and return it.
   function Read_Hash (User : String) return String;

   --  Compare a hash string from /etc/passwd with raw unencrypted input.
   --  Return true if the second hashes to the first.
   function Match (Hash : String; Key : String) return Boolean;

   No_Entry : exception;
   Invalid_Hash_Field : exception;
   Crypt_Failure : exception;
private
   function Match_Line (Line : String; User : String) return Boolean;

   function Get_Hash_Field (Line : String) return String;

   function Hash_Text (Key : String; Data : String) return String;

   function Crypt (key : chars_ptr; salt : chars_ptr) return chars_ptr;

   function Crypt_Wrapper (Key : chars_ptr; Data : chars_ptr) return chars_ptr;

   function Get_Salt_Split (Hash : String) return Natural;

   pragma Import
      (Convention => C, Entity => Crypt, External_Name => "crypt");
end Shadow;
