with Interfaces.C;
use Interfaces.C;

package Pass is
   --  Prompt the user for their password, return true if it matches the
   --  password database.
   function Verify (User : String) return Boolean;

   Not_A_TTY : exception;

   Cannot_Modify_TTY : exception;

   Cannot_Read : exception;
private
   function Read return String;

   function Term_Init return int;

   function Echo_Enable return int;

   function Echo_Disable return int;

   pragma Import
      (Convention => C, Entity => Term_Init, External_Name => "term_init");

   pragma Import
      (Convention => C,
      Entity => Echo_Enable,
      External_Name => "term_echo_enable");

   pragma Import
      (Convention => C,
      Entity => Echo_Disable,
      External_Name => "term_echo_disable");
end Pass;
