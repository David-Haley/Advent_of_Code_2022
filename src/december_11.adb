with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use  Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_11 is

   subtype Worries is Unsigned_64;

   package Item_Stores is new Ada.Containers.Vectors (Positive, Worries);
   use Item_Stores;

   type Operators is (Add, Add_Immediate, Multiply, Multiply_Immediate);

   subtype Monkey_Indices is Natural;

   type Destinations is array (Boolean) of Monkey_Indices;

   type Monkeys is record
      Item_Store : Item_Stores.Vector := Item_Stores.Empty_Vector;
      Immediate : Worries := 0;
      Operator : Operators;
      Modulus : Positive;
      Destination : Destinations;
      Inspections : Unsigned_64 := 0;
   end record; -- Monkeys

   Format_Error : exception;

   package Monkey_Stores is new
     Ada.Containers.Vectors (Monkey_Indices, Monkeys);
   use Monkey_Stores;

   function "<" (Left, Right : Monkeys) return Boolean is

   begin -- "<"
      return Left.Inspections < Right.Inspections;
   end "<";

   package Monkey_Sort is new Monkey_Stores.Generic_Sorting;
   Use Monkey_Sort;

   procedure Read_Input (Monkey_Store : out Monkey_Stores.Vector) is

      Space_Set : Character_Set := To_Set (' ');
      Operator_Set : Character_Set := To_Set ("+*");
      Input_File : File_Type;
      Text : Unbounded_String;
      Monkey : Monkeys;
      Start_At, First : Positive;
      Last : Natural;
      OP_Ch : Character;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_11.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Monkey_Store);
      while not End_Of_File (Input_File) loop
         loop -- find "Monkey"
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
            exit when Slice (Text, First, Last) = "Monkey";
         end loop; -- find "Monkey"
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
         if Slice (Text, First, Last) /= "Starting" then
            raise Format_Error with "Expected ""Starting"" and found """ &
              Slice (Text, First, Last) & '"';
         end if; -- Slice (Text, First, Last) /= "Starting"
         Start_At := Last + 1;
         Clear (Monkey.Item_Store);
         loop -- items
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            exit when Last = 0;
            Append (Monkey.Item_Store,
                    Worries'Value (Slice (Text, First, Last)));
            Start_At := Last + 1;
         end loop; -- items
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
         if Slice (Text, First, Last) /= "Operation:" then
            raise Format_Error with "Expected ""Operation:"" and found """ &
              Slice (Text, First, Last) & '"';
         end if; -- Slice (Text, First, Last) /= "Operation:"
         Start_At := Last + 1;
         Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
         if Slice (Text, First, Last) /= "new" then
            raise Format_Error with "Expected ""new"" and found """ &
              Slice (Text, First, Last) & '"';
         end if; -- Slice (Text, First, Last) /= "new"
         Start_At := Last + 1;
         Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
         if Slice (Text, First, Last) /= "old" then
            raise Format_Error with "Expected ""old"" and found """ &
              Slice (Text, First, Last) & '"';
         end if; -- Slice (Text, First, Last) /= "old"
         Start_At := Last + 1;
         Find_Token (Text, Operator_Set, Start_At, Inside, First, Last);
         OP_Ch := Element (Text, First);
         Start_At := Last + 1;
         Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
         if Slice (Text, First, Last) = "old" then
            case OP_Ch is
               when '+' =>
                  Monkey.Operator := Add;
               when '*' =>
                  Monkey.Operator := Multiply;
               when others =>
                  raise Format_Error with "Bad operator '" & OP_Ch & "'";
            end case; -- OP_Ch
            Monkey.Immediate := 0;
         else
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            if Last > 0 then
               case OP_Ch is
               when '+' =>
                  Monkey.Operator := Add_Immediate;
               when '*' =>
                  Monkey.Operator := Multiply_Immediate;
               when others =>
                  raise Format_Error with "Bad immediate operator '" & OP_Ch
                    & "'";
               end case; -- OP_Ch
               Monkey.Immediate := Worries'Value (Slice (Text, First, Last));
            else
               raise Format_Error with "Missing immediate value";
            end if; -- Last > 0
         end if; -- Slice (Text, First, Last) = "old"
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
         if Slice (Text, First, Last) /= "Test:" then
            raise Format_Error with "Expected ""Test:"" and found """ &
              Slice (Text, First, Last) & '"';
         end if; -- Slice (Text, First, Last) /= "Test:"
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Monkey.Modulus := Positive'Value (Slice (Text, First, Last));
         for D in reverse Boolean loop
            Get_Line (Input_File, Text);
            Start_At := 1;
            Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
            if Slice (Text, First, Last) /= "If" then
               raise Format_Error with "Expected ""If"" and found """ &
                 Slice (Text, First, Last) & '"';
            end if; -- Slice (Text, First, Last) /= "If"
            Start_At := Last + 1;
            Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
            if Boolean'Value (Slice (Text, First, Last)) /= D then
               raise Format_Error with "Unexpected destination order";
            end if; -- Boolean'Value (Slice (Text, First, Last)) /= D
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Monkey.Destination (D) :=
              Monkey_Indices'Value (Slice (Text, First, Last));
         end loop; -- D in Boolean
         Append (Monkey_Store, Monkey);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Update (Monkey_Store : in out Monkey_Stores.Vector;
                     Worry_Divisor : Positive := 3) is

      Old_Worry, New_Worry : Worries;
      Worry_LCM : Worries := 1;
      Test : Boolean;

   begin -- Update
      for M in Iterate (Monkey_Store) loop
         Worry_LCM := Worry_LCM * Worries (Monkey_Store (M).Modulus);
      end loop; -- M in Iterate (Monkey_Store)
      for M in Iterate (Monkey_Store) loop
         While not Is_Empty (Monkey_Store (M).Item_Store) loop
            Monkey_Store (M).Inspections := Monkey_Store (M).Inspections + 1;
            Old_Worry := First_Element (Monkey_Store (M).Item_Store);
            Delete_First (Monkey_Store (M).Item_Store);
            case Monkey_Store (M).Operator is
               when Add =>
                  New_Worry := (Old_Worry + Old_Worry) mod Worry_LCM;
               when Add_Immediate =>
                  New_Worry :=
                    (Old_Worry + Monkey_Store (M).Immediate) mod Worry_LCM;
               when Multiply =>
                  New_Worry := (Old_Worry * Old_Worry) mod Worry_LCM;
               when Multiply_Immediate =>
                  New_Worry :=
                    (Old_Worry * Monkey_Store (M).Immediate) mod Worry_LCM;
            end case; -- Monkey_Store (M).Operator
            New_Worry := New_Worry / Worries (Worry_Divisor);
            Test := New_Worry mod Worries (Monkey_Store (M).Modulus) = 0;
            Append (Monkey_Store (Monkey_Store (M).Destination (Test)).
                      Item_Store, New_Worry);
         end loop; -- Is_Empty (Monkey_Store (M).Item_Store)
      end loop; -- M in Iterate (Monkey_Store)
   end Update;

   Monkey_Store : Monkey_Stores.Vector;

begin -- December_11
   Read_Input (Monkey_Store);
   for R in Positive range 1 .. 20 loop
      Update (Monkey_Store);
   end loop; -- R in Positive range 1 .. 20
   Sort (Monkey_Store);
   Put_Line ("Part one:" &
               Unsigned_64'Image (Last_Element (Monkey_Store).Inspections *
                 Monkey_Store (Last_Index (Monkey_Store) - 1).Inspections));
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Monkey_Store);
   for R in Positive range 1 .. 10000 loop
      Update (Monkey_Store, 1);
   end loop; -- R in Positive range 1 .. 10000
   Sort (Monkey_Store);
   Put_Line ("Part two:" &
               Unsigned_64'Image (Last_Element (Monkey_Store).Inspections *
                 Monkey_Store (Last_Index (Monkey_Store) - 1).Inspections));
   DJH.Execution_Time.Put_CPU_Time;
end December_11;
