with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_04 is

   subtype Sections is Positive range 1 .. 99;

   package Section_Sets is new Ada.Containers.Ordered_Sets (Sections);
   use Section_Sets;

   subtype Elf_Indices is Positive range 1 .. 2;

   type Elf_Pairs is array (Elf_Indices) of Section_Sets.Set;

   subtype Pair_Indices is Positive;

   package Pair_Stores is new Ada.Containers.Vectors (Pair_Indices, Elf_Pairs);
   use Pair_Stores;

   Format_Error : exception;

   procedure Read_Input (Pair_Store : out Pair_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First, Low, High: Positive;
      Last : Natural;
      Elf_Pair : Elf_Pairs;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_04.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for E in Elf_Indices loop
            Clear (Elf_Pair (E));
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Low := Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            if Element (Text, Start_AT) /= '-' then
               raise Format_Error with "expected '-' and found '" &
                 Element (Text, Start_AT) & "'";
            end if; -- Element (Text, Start_AT) /= '-'
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            High := Positive'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            if E = Elf_Indices'First and then
              Element (Text, Start_AT) /= ',' then
               raise Format_Error with "expected ',' and found '" &
                 Element (Text, Start_AT) & "'";
            end if; -- E = Elf_Indices'First and then ...
            for S in Positive range Low .. High loop
               Include (Elf_Pair (E), S);
            end loop; -- S in Positive range Low .. High
         end loop; -- E in Elf_Indices
         Append (Pair_Store, Elf_Pair);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   Pair_Store : Pair_Stores.Vector;
   Overlap_Count_1, Overlap_Count_2: Natural := 0;

begin -- December_04
   Read_Input (Pair_Store);
   for P in Iterate (Pair_Store) loop
      If Is_Subset (Pair_Store (P) (1), Pair_Store (P) (2)) or
        Is_Subset (Pair_Store (P) (2), Pair_Store (P) (1)) then
         Overlap_Count_1 := Overlap_Count_1 + 1;
      end if; -- Is_Subset (Pair_Store (P) (1), Pair_Store (P) (2)) or ...
      If Overlap (Pair_Store (P) (1), Pair_Store (P) (2)) or
        Is_Subset (Pair_Store (P) (2), Pair_Store (P) (1)) then
         Overlap_Count_2 := Overlap_Count_2 + 1;
      end if; --  Overlap (Pair_Store (P) (1), Pair_Store (P) (2)) or ...
   end loop; -- P in Iterate (Pair_Store)
   Put_Line ("Part one:" & Overlap_Count_1'Img);
   Put_Line ("Part two:" & Overlap_Count_2'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_04;
