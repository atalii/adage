with Interfaces.C;
use Interfaces.C;

package Pass is
   function Read return String;

   Not_A_TTY : exception;

   Cannot_Modify_TTY : exception;
private
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
