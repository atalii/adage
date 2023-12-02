package Conf is
   type Options is record
      No_Pass : Boolean;
      Keep_Env : Boolean;
   end record;

   type Ticket is record
      Permit : Boolean;
      Opts : Options;
   end record;
end Conf;
