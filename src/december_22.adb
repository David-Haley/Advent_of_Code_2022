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
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype Ordinates is Natural;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   type Jungle_Elements is (Path, Wall);

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   package Jungle_Maps is new
     Ada.Containers.Ordered_Maps (Coordinates, Jungle_Elements);
   use  Jungle_Maps;

   type Directions is mod 4;
   -- CW rotation order starting from East
   East : constant Directions := 0;
   South : constant Directions := 1;
   West : constant Directions := 2;
   North : constant Directions := 3;

   type Positions is record
      Coordinate: Coordinates := (1, 1);
      Facing : Directions := East;
   end record; -- Positions

   type Turns is (ACW, CW, None);

   type Instructions is record
      Turn : Turns;
      Steps : Positive;
   end record; -- Instructions

   package Instruction_Lists is new
     Ada.Containers.Vectors (Positive, Instructions);
   use Instruction_Lists;

   package Vertex_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Vertex_Sets;

   function "<" (Left, Right : Positions) return Boolean is
     (Left.Coordinate.X < Right.Coordinate.X or (
      Left.Coordinate.X = Right.Coordinate.X and
        (Left.Coordinate.Y < Right.Coordinate.Y or
           (Left.Coordinate.Y = Right.Coordinate.Y and
                Left.Facing < Right.Facing))));

   package Edge_Pairs is new
     Ada.Containers.Ordered_Maps (Positions, Coordinates);
   use Edge_Pairs;

   procedure Read_Input (Jungle_Map : out Jungle_Maps.Map;
                         Instructions_List : out Instruction_Lists.Vector) is

      Turn_Set : constant Character_Set := To_Set ("LR");
      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := 1;
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
                  Position.Facing := @ - 1;
               when CW =>
                  Position.Facing := @ + 1;
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
               when East =>
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
               when West =>
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
        Ordinates (Position.Facing);
   end Walk_Path;

   function Side_Length (Jungle_Map : in Jungle_Maps.Map) return Positive is

      X_Min, Y_Min, Side : Ordinates := Ordinates'Last;
      X_Max, Y_Max : Ordinates := Ordinates'First;
      Side_Min, Side_Max : Ordinates;

   begin -- Side_Length
      for J in Iterate (Jungle_Map) loop
         if Key (J).X < X_Min then
            X_Min := Key (J).X;
         end if; -- Key (J).X < X_Min
         if Key (J).X > X_Max then
            X_Max := Key (J).X;
         end if; -- Key (J).X > X_Max
         if Key (J).Y < Y_Min then
            Y_Min := Key (J).Y;
         end if; -- Key (J).Y < Y_Min
         if Key (J).Y > Y_Max then
            Y_Max := Key (J).Y;
         end if; -- Key (J).Y > Y_Max
      end loop; -- J in Iterate (Jungle_Map)
      -- find the mimimum width containing Jungle_Map
      for Y in Ordinates range Y_Min .. Y_Max loop
         Side_Min := Ordinates'Last;
         Side_Max := Ordinates'First;
         for X in Ordinates range X_Min .. X_Max loop
            if Contains (Jungle_Map, (X, Y)) then
               if X < Side_Min then
                  Side_Min := X;
               end if; -- X < Side_Min
               if X > Side_Max then
                  Side_Max := X;
               end if; -- X > Side_Max
            end if; -- Contains (Jungle_Map, (X, Y, 0))
         end loop; -- X in Ordinates range X_Min .. X_Max
         if Side > Side_Max - Side_Min + 1 then
            Side := Side_Max - Side_Min + 1;
         end if; -- Side > Side_Max - Side_Min + 1
      end loop; -- Y in Ordinates range Y_Min .. Y_Max
      return Side;
   end Side_Length;

   procedure Find_Edges (Jungle_Map : in Jungle_Maps.Map;
                         Edge_Pair : out Edge_Pairs.Map;
                         Internal_Vertex : out Vertex_Sets.Set;
                         External_Vertex : out Vertex_Sets.Set) is

      Neighbours : Natural;
      Dummy : constant Coordinates := (Ordinates'First, Ordinates'First);
      -- Coordinates of the adjoining edge not Known.

   begin -- Find_Edges
      Clear (Edge_Pair);
      Clear (Internal_Vertex);
      Clear (External_Vertex);
      for I in Iterate (Jungle_Map) loop
         Neighbours := 0;
         if Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y)) then
            Neighbours := @ + 1;
         else
            Insert (Edge_Pair, (Key (I), West), Dummy);
         end if; -- Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y))
         if Contains (Jungle_Map, (Key (I).X + 1, Key (I).Y)) then
            Neighbours := @ + 1;
         else
            Insert (Edge_Pair, (Key (I), East), Dummy);
         end if; -- Contains (Jungle_Map, (Key (I).X + 1, Key (I).Y))
         if Contains (Jungle_Map, (Key (I).X, Key (I).Y - 1)) then
            Neighbours := @ + 1;
         else
            Insert (Edge_Pair, (Key (I), North), Dummy);
         end if; -- Contains (Jungle_Map, (Key (I).X, Key (I).Y - 1))
         if Contains (Jungle_Map, (Key (I).X, Key (I).Y + 1)) then
            Neighbours := @ + 1;
         else
            Insert (Edge_Pair, (Key (I), South), Dummy);
         end if; -- Contains (Jungle_Map, (Key (I).X, Key (I).Y + 1))
         -- Count diagonal neighbours
         if Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y + 1)) then
            Neighbours := @ + 1;
         end if; -- Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y + 1))
         if Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y - 1)) then
            Neighbours := @ + 1;
         end if; -- Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y - 1)
         if Contains (Jungle_Map, (Key (I).X + 1, Key (I).Y + 1)) then
            Neighbours := @ + 1;
         end if; -- Contains (Jungle_Map, (Key (I).X - 1, Key (I).Y + 1))
         if Contains (Jungle_Map, (Key (I).X + 1, Key (I).Y - 1)) then
            Neighbours := @ + 1;
         end if; -- Contains (Jungle_Map, (Key (I).X + 1, Key (I).Y + 1))
         if Neighbours = 3 then
            Include (External_Vertex, Key (I));
         elsif Neighbours = 7 then
            Include (Internal_Vertex, Key (I));
         end if; -- Neighbours = 3
      end loop; -- I in Iterate (Jungle_Map)
   end Find_Edges;

   procedure Match (Internal_Vertex : out Vertex_Sets.Set;
                    External_Vertex : out Vertex_Sets.Set;
                    Edge_Pair : in out Edge_Pairs.Map) is

      function Next (Edge_Pair : in Edge_Pairs.Map;
                     Ec : in Edge_Pairs.Cursor;
                     Direction : in Directions) return Edge_Pairs.Cursor is

         -- Position need not be a member of the edge set, that is, it may be an
         -- internal vertex.

         Test_Position : Positions := Key (Ec);

      begin -- Next
         if Test_Position.Facing /= Direction + 1 and
           Test_Position.Facing /= Direction - 1 then
            raise Program_Error with "Search direction not along edge";
         end if; -- Test_Position.Facing /= Direction + 1 and ...
         case Direction is
         when East =>
            Test_Position.Coordinate.X := @ + 1;
         when West =>
            Test_Position.Coordinate.X := @ - 1;
         when North =>
            Test_Position.Coordinate.Y := @ - 1;
         when South =>
            Test_Position.Coordinate.Y := @ + 1;
         end case; -- Direction
         return Find (Edge_Pair, Test_Position);
      end Next;

   begin -- Match
   end Match;

   Jungle_Map : Jungle_Maps.Map;
   Instruction_List : Instruction_Lists.Vector;
   Side : Ordinates;
   Edge_Pair : Edge_Pairs.Map;
   Internal_Vertex, External_Vertex : Vertex_Sets.Set;

begin -- December_22
   Read_Input (Jungle_Map, Instruction_List);
   Put_Line ("Part one:" & Walk_Path (Jungle_Map, Instruction_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Side := Side_Length (Jungle_Map);
   Find_Edges (Jungle_Map, Edge_Pair, Internal_Vertex, External_Vertex);
   Put_Line ("Edges:");
   for E in Iterate (Edge_Pair) loop
      Put_Line (Key (E).Coordinate.X'Img & Key (E).Coordinate.Y'Img & Key (E).Facing'Img);
   end loop;
   Put_Line ("Internal Vertices");
   for V in Iterate (Internal_Vertex) loop
      Put_Line (Element (V).X'Img & Element (V).Y'Img);
   end loop;
   Put_Line ("External Vertices");
   for V in Iterate (External_Vertex) loop
      Put_Line (Element (V).X'Img & Element (V).Y'Img);
   end loop;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
