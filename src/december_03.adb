with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_03 is

   subtype Compartment_Indices is Positive range 1 .. 2;

   subtype Items is Character with
     Static_Predicate => Items in 'a' .. 'z' | 'A' .. 'Z';

   package Item_Sets is new Ada.Containers.Ordered_Sets (Items);
   use Item_Sets;

   type Rucksacks is array (Compartment_Indices) of Item_Sets.Set;

   package Rucksack_Stores is new Ada.Containers.Vectors (Positive, Rucksacks);
   use Rucksack_Stores;

   procedure Read_Input (Rucksack_Store : out Rucksack_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Rucksack : Rucksacks;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_03.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Length (Text) mod 2 /= 0 then
            raise Constraint_Error with "Odd number of items" &
              Length (Text)'Img;
         end if; -- Length (Text) mod 2 /= 0
         Clear (Rucksack (1));
         Clear (Rucksack (2));
         for I in Positive range 1 .. Length (Text) loop
            if I <= Length (Text) / 2 then
               -- Compartment one contents
               Include (Rucksack (1), Element (Text, I));
            else
               -- Compartment two contents
               Include (Rucksack (2), Element (Text, I));
            end if; -- I <= Length (Text) / 2
         end loop; -- I in Positive range 1 .. Length (Text)
         Append (Rucksack_Store, Rucksack);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Priority (Item : in Items) return Positive is

   begin -- Priority
      if 'a' <= Item and Item <= 'z' then
         return Items'Pos (Item) - Items'Pos ('a') + 1;
      else
         return Items'Pos (Item) - Items'Pos ('A') + 27;
      end if; -- 'a' <= Item and Item <= 'z'
   end Priority;

   Rucksack_Store : Rucksack_Stores.Vector;
   Common : Item_Sets.Set;
   Sum_Priority : Natural := 0;
   Group_Size : constant Positive := 3;

begin -- December_03
   Read_Input (Rucksack_Store);
   for R in Iterate (Rucksack_Store) loop
      Common := Intersection (Rucksack_Store (R) (1), Rucksack_Store (R) (2));
      if Length (Common) > 1 then
         raise Constraint_Error with
           "More than one item type common to both compartments";
      end if; -- Length (Common) > 1
      Sum_Priority := Sum_Priority + Priority (First_Element(Common));
   end loop; -- R in Iterate (Rucksack_Store)
   Put_Line ("Part one:" & Sum_Priority'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Sum_Priority := 0;
   for R in Positive range 1 .. Last_Index (Rucksack_Store) loop
      if (R - 1) mod Group_Size = 0 then
         Common := Union (Rucksack_Store (R) (1), Rucksack_Store (R) (2));
      else
         Common := Intersection (Common, Union (Rucksack_Store (R) (1),
                                                Rucksack_Store (R) (2)));
      end if; -- (R - 1) mod Group_Size = 0
      if (R - 1) mod Group_Size = Group_Size - 1 then
         if Length (Common) > 1 then
            raise Constraint_Error with
              "More than one item type carried by all three elves in group";
         end if; -- Length (Common) > 1
         Sum_Priority := Sum_Priority + Priority (First_Element (Common));
      end if; -- (R - 1) mod Group_Size = Group_Size - 1
   end loop; -- R in Positive range 1 .. Last_Index (Rucksack_Store)
   Put_Line ("Part two:" & Sum_Priority'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_03;
