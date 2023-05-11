with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Ident;
use Ident;

--  We can parse all of /etc/adage.conf except for :options.
package Conf is
   type Options is record
      No_Pass : Boolean;
      Keep_Env : Boolean;
   end record;

   type Ticket is record
      Permit : Boolean;
      Opts : Options;
   end record;

    -- Read the rules from /etc/adage.conf. Returns permitted if everything's
    -- okay, and returns false if there's been an error, at which point one can
    -- log the errors.
   function Read_Rules return Boolean;

    -- Log all errors. This effectively does nothing when no errors are
    -- present.
   procedure Log_Errors;

   function Is_Permitted
      (Drop_Target : String; Actor_Name : String; Actor_Groups : Groups.Vector)
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
end Conf;
