with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype X_Ordinates is Integer;

   subtype Y_Ordinates is Natural;

   type Coordinates is record
      X : X_Ordinates;
      Y : Y_Ordinates;
   end record; -- Coordinates

   type Jungle_Elements is (Path, Wall);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "=" (Left, Right : Coordinates) return Boolean is
     (Left.X = Right.X and Left.Y = Right.Y);

   package Jungle_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Jungle_Elements);
   use  Jungle_Maps;

   type Directions is (West, South, East, North);
   -- CW rotation order stsrt West

   type Positions is record
      Coordinate: Coordinates := (1, 1);
      Facing : Directions := West;
   end record; -- Positions

   type Turns is (ACW, CW, None);

   type Instructions is record
      Turn : Turns;
      Steps : Positive;
   end record; -- Instructions

   package Instruction_Lists is new
     Ada.Containers.Vectors (Positive, Instructions);
   use Instruction_Lists;

   procedure Read_Input (Jungle_Map : out Jungle_Maps.Map;
                         Instructions_List : out Instruction_Lists.Vector) is

      Turn_Set : constant Character_Set := To_Set ("LR");
      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Y_Ordinates := 1;
      Start_At, First : Positive;
      Last : Natural;
      Instruction : Instructions;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_22.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Jungle_Map);
      loop -- until Length (Text) = 0
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            if Element (Text, I) = '.' then
               Include (Jungle_Map, (I, Y), Path);
            elsif  Element (Text, I) = '#' then
               Include (Jungle_Map, (I, Y), Wall);
            end if; -- Element (Text, I) = '.'
         end loop; -- I : in Positive range 1 .. Length (Text)ext, Jungle_Map);
         exit when Length (Text) = 0;
         Y := Y + 1;
      end loop; -- until Length (Text) = 0
      Clear (Instructions_List);
      Get_Line (Input_File, Text);
      Start_At := 1;
      while Start_At <= Length (Text) loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Start_At := Last + 1;
         Instruction.Steps := Positive'Value (Slice (Text, First, Last));
         Find_Token (Text, Turn_Set, Start_At, Inside, First, Last);
         if Last > 0 then
            -- not last instruction
            Start_At := Last + 1;
            if Element (Text, First) = 'L' then
               Instruction.Turn := ACW;
            elsif Element (Text, First) = 'R' then
               Instruction.Turn := CW;
            else
               raise Program_Error with "Expected 'L' or 'R' and found '" &
                 Element (Text, First) & "'";
            end if; -- Element (Text, First) = 'L'
         else
            Instruction.Turn := None;
         end if; -- Last > 0
         Append (Instructions_List, Instruction);
      end loop; -- Start_At < Length (Text)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Walk_Path (Jungle_Map : in Jungle_Maps.Map;
                       Instruction_List : in Instruction_Lists.Vector)
                       return Positive is

      procedure Move (Instruction_In : Instructions;
                      Position : in out Positions) is

         procedure Turn (Instruction : in Instructions;
                         Position : in out Positions) is

         begin -- Turn
            case Instruction.Turn is
               when ACW =>
                  if Position.Facing = Directions'First then
                     Position.Facing := Directions'Last;
                  else
                     Position.Facing := Directions'Pred (Position.Facing);
                  end if; -- Instruction.Turn = ACW
               when CW =>
                  if Position.Facing = Directions'Last then
                     Position.Facing := Directions'First;
                  else
                     Position.Facing := Directions'Succ (Position.Facing);
                  end if;
               when None =>
                  null; -- last instruction does not have a turn
            end case; -- Instruction.Turn
         end Turn;

         Instruction : Instructions := Instruction_In;
         Test : Coordinates;

      begin -- Move
         loop -- Step
            Test := Position.Coordinate;
            case Position.Facing is
               when West =>
                  Test.X := Test.X + 1;
                  if Contains (Jungle_Map, Test) then
                     if Jungle_Map (Test) = Wall then
                        Test.X := Test.X - 1;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  else
                     -- Wrap around
                     Test.X := Test.X - 1;
                     while Contains (Jungle_Map, Test) loop
                        Test.X := Test.X - 1;
                     end loop; -- Contains (Jungle_Map, Test)
                     Test.X := Test.X + 1;
                     if Jungle_Map (Test) = Wall then
                        Test.X := Position.Coordinate.X;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  end if; -- Contains (Jungle_Map, Test)
               when South =>
                  Test.Y := Test.Y + 1;
                  if Contains (Jungle_Map, Test) then
                     if Jungle_Map (Test) = Wall then
                        Test.Y := Test.Y - 1;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  else
                     -- Wrap around
                     Test.Y := Test.Y - 1;
                     while Contains (Jungle_Map, Test) loop
                        Test.Y := Test.Y - 1;
                     end loop; -- Contains (Jungle_Map, Test)
                     Test.Y := Test.Y + 1;
                     if Jungle_Map (Test) = Wall then
                        Test.Y := Position.Coordinate.Y;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  end if; -- Contains (Jungle_Map, Test)
               when East =>
                  Test.X := Test.X - 1;
                  if Contains (Jungle_Map, Test) then
                     if Jungle_Map (Test) = Wall then
                        Test.X := Test.X + 1;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  else
                     -- Wrap around
                     Test.X := Test.X + 1;
                     while Contains (Jungle_Map, Test) loop
                        Test.X := Test.X + 1;
                     end loop; -- Contains (Jungle_Map, Test)
                     Test.X := Test.X - 1;
                     if Jungle_Map (Test) = Wall then
                        Test.X := Position.Coordinate.X;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  end if; -- Contains (Jungle_Map, Test)
               when North =>
                  Test.Y := Test.Y - 1;
                  if Contains (Jungle_Map, Test) then
                     if Jungle_Map (Test) = Wall then
                        Test.Y := Test.Y + 1;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  else
                     -- Wrap around
                     Test.Y := Test.Y + 1;
                     while Contains (Jungle_Map, Test) loop
                        Test.Y := Test.Y + 1;
                     end loop; -- Contains (Jungle_Map, Test)
                     Test.Y := Test.Y - 1;
                     if Jungle_Map (Test) = Wall then
                        Test.Y := Position.Coordinate.Y;
                        Instruction.Steps := 1;
                     end if; -- Jungle_Map (Test) = Wall
                  end if; -- Contains (Jungle_Map, Test)
            end case; -- Position.Facing
            Position.Coordinate := Test;
            exit when Instruction.Steps = 1;
            Instruction.Steps := Instruction.Steps - 1;
         end loop; -- Step
         Turn (Instruction, Position);
      end Move;

      Position : Positions;

   begin -- Walk_Path
      loop -- find start of path
         exit when Contains (Jungle_Map, Position.Coordinate) and then
           Jungle_Map ( Position.Coordinate) = Path;
         Position.Coordinate.X := Position.Coordinate.X + 1;
      end loop; -- find start of path
      for I in Iterate (Instruction_List) loop
         Move (Instruction_List (I), Position);
      end loop; -- I in Iterate (Instruction_List)
      return 1000 * Position.Coordinate.Y + 4 * Position.Coordinate.X +
        Directions'Pos (Position.Facing);
   end Walk_Path;

   Jungle_Map : Jungle_Maps.Map;
   Instruction_List : Instruction_Lists.Vector;

begin -- December_22
   Read_Input (Jungle_Map, Instruction_List);
   Put_Line ("Part one:" & Walk_Path (Jungle_Map, Instruction_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
