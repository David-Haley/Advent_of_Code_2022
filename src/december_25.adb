with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_25 is

   subtype Numbers is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   SNAFU_Base : constant Numbers := 5;

   subtype SNAFU_Digits is Character with
     Static_Predicate => SNAFU_Digits in '=' | '-' | '0' | '1' | '2';

   package SNAFU_Stores is new
     Ada.Containers.Vectors (Positive, Unbounded_String);
  use SNAFU_Stores;

   procedure Read_Input (SNAFU_Store : out SNAFU_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_25.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (SNAFU_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (SNAFU_Store, Trim (Text, Both));
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function SNAFU_to_Number (Text : in Unbounded_String) return Numbers is

      Ch : SNAFU_Digits;
      Number : Numbers := 0;

   begin -- SNAFU_to_Number
      for I in Positive range 1 .. Length (Text) loop
         Ch :=Element (Text, I);
         Number := Number * SNAFU_Base;
         case Ch is
            when '=' =>
               Number := Number - 2;
            when '-' =>
               Number := Number - 1;
            when '0' =>
               null;
            when '1' =>
               Number := Number + 1;
            when '2' =>
               Number := Number + 2;
         end case; -- Ch
      end loop; -- I in Positive range 1 .. Length (Text)
      return Number;
   end SNAFU_to_Number;

   function Number_to_SNAFU (Number_In : in Numbers) return Unbounded_String is

      Number : Numbers := Number_In;
      SNAFU : Unbounded_String := Null_Unbounded_String;

   begin -- Number_to_SNAFU
      while Number > 0 loop
         case Number mod SNAFU_Base is
            when 0 =>
               Insert (SNAFU, 1, "0");
            when 1 =>
               Insert (SNAFU, 1, "1");
            when 2 =>
               Insert (SNAFU, 1, "2");
            when 3 =>
               Insert (SNAFU, 1, "=");
               Number := Number + 5;
            when 4 =>
               Insert (SNAFU, 1, "-");
               Number := Number + 5;
            when others =>
               null;
         end case; -- Number mod SNAFU_Base
         Number := Number / SNAFU_Base;
      end loop; -- Number > 0
      return SNAFU;
   end Number_to_SNAFU;

   SNAFU_Store : SNAFU_Stores.Vector;
   Sum : Numbers := 0;

begin -- December_25
   Read_Input (SNAFU_Store);
   for S in Iterate (SNAFU_Store) loop
      Sum := Sum + SNAFU_to_Number (SNAFU_Store (S));
   end loop; -- S in SNAFU_Store
   Put_Line ("Part one: " & Number_to_SNAFU (SUM));
   DJH.Execution_Time.Put_CPU_Time;
end December_25;
