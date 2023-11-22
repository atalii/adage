with Ada.Containers.Vectors;

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ident;
use Ident;

package Conf is
   type Options is record
      No_Pass : Boolean;
      Keep_Env : Boolean;
   end record;

   type Ticket is record
      Permit : Boolean;
      Opts : Options;
   end record;

   type Rules is tagged private;

   Parse_Failure : exception;

   function Read_Rules return Rules;
   --  Read the rules from /etc/adage.conf. This sets some package-global
   --  variables and throws if an error is encountered while parsing the config
   --  file. Log_Errors may be used to display the errors in such a case.

   procedure Log_Errors;
   --  Log all errors. This effectively does nothing when no errors are
   --  present.

   function Is_Permitted
      (R : Rules;
       Drop_Target : String;
       Actor_Name : String;
       Actor_Groups : Groups.Vector)
      return Ticket;
private
   type Rule_Effect is (Permit, Reject);
   type Category is (User, Group);

   --  A rule pertains to a target actor and allows or disallows access to a
   --  drop actor. Where the target actor may be a user or group, the drop
   --  actor must be a gorup.
   type Rule is record
      Effect : Rule_Effect;
      Target_Category : Category;
      Target_Actor : Unbounded_String;
      Drop_Actor : Unbounded_String;
      Opts : Options;
   end record;

   package Rules_Vec is new
      Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Rule);

   type Rules is tagged record
      Vec : Rules_Vec.Vector;
   end record;

   function Check_Conf_Perms return Integer;

   pragma Import
      (Convention => C,
      Entity => Check_Conf_Perms,
      External_Name => "check_conf_perms");
end Conf;
