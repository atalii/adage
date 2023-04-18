with Ada.Containers.Indefinite_Vectors;

package Ident is
   package Groups is new Ada.Containers.Indefinite_Vectors
      (Index_Type => Natural,
       Element_Type => String);

   function Read_User return String;

   function Read_Groups return Groups.Vector;
end Ident;
