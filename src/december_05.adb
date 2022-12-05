with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_05 is

   subtype Crates is Character range 'A' .. 'Z';

   subtype Heights is Positive;

   package Stacks is new Ada.Containers.Vectors (Heights, Crates);
   use Stacks;

   subtype Stack_Indices is Positive range 1 .. 9;

   type Stack_Arrays is array (Stack_Indices) of Stacks.Vector;

   type Moves is record
      Quantity : Heights;
      Source, Destination : Stack_Indices;
   end record; -- Moves

   package Move_Stores is new Ada.Containers.Vectors (Positive, Moves);
   use Move_Stores;

   procedure Read_Input (Stack_Array : out Stack_Arrays;
                         Move_Store : out Move_Stores.Vector) is

      -- N.B. No error checking of input.

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Stack : Stack_Indices;
      Move : Moves;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_05.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      -- Theoretically the following two lines should not be required; however
      -- without these the previous content is retained for part 2.
      Stack_Array := (others => Stacks.Empty_Vector);
      Clear (Move_Store);
      loop -- read one layer
         Get_Line (Input_File, Text);
         Start_At := 1;
         loop -- read one crate from stack
            Find_Token (Text, Upper_Set, Start_At, Inside, First, Last);
            exit when Last = 0;
            Stack := (First - 2) / 4 + 1; -- allows for "] [" between stscks
            Insert (Stack_Array (Stack), 1, Element (Text, First));
            Start_At := Last + 1;
         end loop; -- read one crate from stack
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         exit when Last /= 0 and then Element (Text, First) = '1';
      end loop; -- read one layer
      Skip_Line (Input_File);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Move.Quantity := Positive'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Move.Source := Stack_Indices'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Move.Destination := Stack_Indices'Value (Slice (Text, First, Last));
         Append (Move_Store, Move);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   procedure Crate_Mover_9000 (Stack_Array : in out Stack_Arrays;
                               Move : in Moves) is

   begin -- Crate_Mover_9000
      for Q in Positive range 1 .. Move.Quantity loop
         Append (Stack_Array (Move.Destination),
                 Last_Element (Stack_Array (Move.Source)));
         Delete_Last (Stack_Array (Move.Source));
      end loop; -- Q in Positive range 1 .. Move.Quantity
   end Crate_Mover_9000;

   procedure Crate_Mover_9001 (Stack_Array : in out Stack_Arrays;
                               Move : in Moves) is

   begin -- Crate_Mover_9001
      for Q in reverse Positive range 1 .. Move.Quantity loop
         Append (Stack_Array (Move.Destination),
                 Stack_Array (Move.Source)
                 (Last_Index (Stack_Array (Move.Source)) + 1 - Q));
      end loop; -- Q in Positive range 1 .. Move.Quantity
      Delete_Last (Stack_Array (Move.Source), Count_Type (Move.Quantity));
   end Crate_Mover_9001;

   function Top_Crates (Stack_Array : in Stack_Arrays) Return String is

      Result : String (Stack_Indices);

   begin -- Top_Crates
      for S in Stack_Indices loop
         -- Test required to allow code to work for fewer than nine stacks.
         if Length (Stack_Array (S)) > 0 then
            Result (S) := Last_Element (Stack_Array (S));
         else
            Result (S) := ' ';
         end if; -- Length (Stack_Array (S)) > 0
      end loop; -- S in Stack_Indices
      return Result;
   end Top_Crates;

   Stack_Array : Stack_Arrays;
   Move_Store : Move_Stores.Vector;

begin -- December_05
   Read_Input (Stack_Array, Move_Store);
   for M in iterate (Move_Store) loop
      Crate_Mover_9000 (Stack_Array, Move_Store (M));
   end loop; -- M in iterate (Move_Store);
   Put_Line ("Part one: " & Top_Crates (Stack_Array));
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Stack_Array, Move_Store);
   for M in iterate (Move_Store) loop
      Crate_Mover_9001 (Stack_Array, Move_Store (M));
   end loop; -- M in iterate (Move_Store);
   Put_Line ("Part two: " & Top_Crates (Stack_Array));
   DJH.Execution_Time.Put_CPU_Time;
end December_05;
