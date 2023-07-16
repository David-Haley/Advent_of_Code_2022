with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
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
   D90 : constant Directions := 1; -- add or subtract for 90 degrees
   D180 : constant Directions := 2; -- add for reversal in direction

   type Positions is record
      Coordinate: Coordinates := (1, 1);
      Facing : Directions := East;
   end record; -- Positions

   Dummy : constant Positions := ((Ordinates'First, Ordinates'First),
                                  Directions'First);
   -- Coordinates and Direction of the adjoining edge not Known.

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
     Ada.Containers.Ordered_Maps (Positions, Positions);
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

   procedure Find_Edges (Jungle_Map : in Jungle_Maps.Map;
                         Edge_Pair : out Edge_Pairs.Map;
                         Internal_Vertex : out Vertex_Sets.Set;
                         External_Vertex : out Vertex_Sets.Set) is

      Neighbours : Natural;

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

   procedure Wrap_Match (Jungle_Map : in Jungle_Maps.Map;
                         Edge_Pair : out Edge_Pairs.Map) is

      -- Match pairs for part one, matches to opposide edge of map either
      -- horizontally or vertically.

      Test : Coordinates;

   begin -- Wrap_Match
      for Ec in Iterate (Edge_Pair) loop
         Test := Key (Ec).Coordinate;
         case Key (Ec).Facing is
            when East =>
               while Contains (Jungle_Map, Test) loop
                  Test.X := @ - 1;
               end loop; -- Contains (Jungle_Map, Test)
               Test.X := @ + 1;
            when South =>
               while Contains (Jungle_Map, Test) loop
                  Test.Y := @ - 1;
               end loop; -- Contains (Jungle_Map, Test)
               Test.Y := @ + 1;
            when West =>
               while Contains (Jungle_Map, Test) loop
                  Test.X := @ + 1;
               end loop; -- Contains (Jungle_Map, Test)
               Test.X := @ - 1;
            when North =>
               while Contains (Jungle_Map, Test) loop
                  Test.Y := @ + 1;
               end loop; -- Contains (Jungle_Map, Test)
               Test.Y := @ - 1;
         end case; -- Key (Ec).Facing
         Edge_Pair (Ec) := (Test, Key (Ec).Facing);
      end loop; -- Ec in Iterate (Edge_Pair)
   end Wrap_Match;

   function Walk_Path (Jungle_Map : in Jungle_Maps.Map;
                       Instruction_List : in Instruction_Lists.Vector;
                       Edge_Pair : in Edge_Pairs.Map)
                       return Positive is

      procedure Move (Instruction_In : Instructions;
                      Position : in out Positions;
                      Edge_Pair : in Edge_Pairs.Map) is

         Instruction : Instructions := Instruction_In;
         Test : Coordinates;
         New_Direction : Directions;

      begin -- Move
         loop -- Step
            Test := Position.Coordinate;
            case Position.Facing is
               when East =>
                  Test.X := Test.X + 1;
               when South =>
                  Test.Y := Test.Y + 1;
               when West =>
                  Test.X := Test.X - 1;
               when North =>
                  Test.Y := Test.Y - 1;
            end case; -- Position.Facing
            if Contains (Jungle_Map, Test) then
               if Jungle_Map (Test) = Wall then
                  Test := Position.Coordinate;
                  Instruction.Steps := 1;
               end if; -- Jungle_Map (Test) = Wall
            else
               -- Off map
               Test := Position.Coordinate;
               -- Use Edge_Pair to find new map coordinate and direction (part
               -- two), direction is unchanged when wrap map used.
               New_Direction := Edge_Pair ((Test, Position.Facing)).Facing;
               Test := Edge_Pair ((Test, Position.Facing)).Coordinate;
               if Jungle_Map (Test) = Wall then
                  Test := Position.Coordinate;
                  Instruction.Steps := 1;
               else
                  Position.Facing := New_Direction;
               end if; -- Jungle_Map (Test) = Wall
            end if; -- Contains (Jungle_Map, Test)
            Position.Coordinate := Test;
            exit when Instruction.Steps = 1;
            Instruction.Steps := Instruction.Steps - 1;
         end loop; -- Step
         case Instruction.Turn is
            when ACW =>
               Position.Facing := @ - D90;
            when CW =>
               Position.Facing := @ + D90;
            when None =>
               null; -- last instruction does not have a turn
         end case; -- Instruction.Turn
      end Move;

      Position : Positions;

   begin -- Walk_Path
      loop -- find start of path
         exit when Contains (Jungle_Map, Position.Coordinate) and then
           Jungle_Map ( Position.Coordinate) = Path;
         Position.Coordinate.X := Position.Coordinate.X + 1;
      end loop; -- find start of path
      for I in Iterate (Instruction_List) loop
         Move (Instruction_List (I), Position, Edge_Pair);
      end loop; -- I in Iterate (Instruction_List)
      return 1000 * Position.Coordinate.Y + 4 * Position.Coordinate.X +
        Ordinates (Position.Facing);
   end Walk_Path;

   procedure Cube_Match (Internal_Vertex : in Vertex_Sets.Set;
                         External_Vertex : in Vertex_Sets.Set;
                         Edge_Pair : in out Edge_Pairs.Map) is

      -- Used by part two to match edge elements when wrapped around a cube.

      type State_Elements is record
         Direction : Directions := Directions'First;
         Change : Boolean := False;
         Ec, Ec_Next : Edge_Pairs.Cursor := Edge_Pairs.No_Element;
      end record; --State_Elements

      type States is array (Boolean) of State_Elements;

      function Next (Edge_Pair : in Edge_Pairs.Map;
                     Ec : in Edge_Pairs.Cursor;
                     Direction : in Directions) return Edge_Pairs.Cursor is

         -- Finds next edge element in search direction, does not follow
         -- direction changes. returns No_Element if none found.

         Test_Position : Positions := Key (Ec);

      begin -- Next
         if Test_Position.Facing /= Direction + D90 and
           Test_Position.Facing /= Direction - D90 then
            raise Program_Error with "Search direction not along edge, start "
              & Key (Ec)'Img & " Direction" & Direction'Img;
         end if; -- Test_Position.Facing /= Direction + D90 and ...
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

      procedure From_Internal (Vertex : in Coordinates;
                               Edge_Pair : in out Edge_Pairs.Map) is

         -- Match edges stepping away from an internal vertex until two changes
         -- in direction are encountered or a vertex that has already been
         -- matched fas been found.

         State : States;
         Finished, Found : Boolean := False;
         Test_Edge : Positions;

      begin -- From_Internal
         for D in Directions loop
            Test_Edge.Facing := D;
            Test_Edge.Coordinate := (Vertex.X - 1, Vertex.Y);
            if Contains (Edge_Pair, Test_Edge) then
               State (Found).Direction := West;
               State (Found).Ec := Find (Edge_Pair, Test_Edge);
               Found := True;
            end if; -- Contains (Edge_Pair, Test_Edge)
            Test_Edge.Coordinate := (Vertex.X + 1, Vertex.Y);
            if Contains (Edge_Pair, Test_Edge) then
               State (Found).Direction := East;
               State (Found).Ec := Find (Edge_Pair, Test_Edge);
               Found := True;
            end if; -- Contains (Edge_Pair, Test_Edge)
            Test_Edge.Coordinate := (Vertex.X, Vertex.Y - 1);
            if Contains (Edge_Pair, Test_Edge) then
               State (Found).Direction := North;
               State (Found).Ec := Find (Edge_Pair, Test_Edge);
               Found := True;
            end if; -- Contains (Edge_Pair, Test_Edge)
            Test_Edge.Coordinate := (Vertex.X, Vertex.Y + 1);
            if Contains (Edge_Pair, Test_Edge) then
               State (Found).Direction := South;
               State (Found).Ec := Find (Edge_Pair, Test_Edge);
               Found := True;
            end if; -- Contains (Edge_Pair, Test_Edge)
         end loop; -- D in Directions
         Edge_Pair (State (True).Ec) := Key (State (False).Ec);
         Edge_Pair (State (True).Ec).Facing := @ + D180;
         Edge_Pair (State (False).Ec) := Key (State (True).Ec);
         Edge_Pair (State (False).Ec).Facing := @ + D180;
         while not Finished loop
            for E in Boolean loop
               State (E).Ec_Next := Next (Edge_Pair, State (E).Ec,
                                          State (E).Direction);
               if State (E).Ec_Next = Edge_Pairs.No_Element then
                  if State (E).Change then
                     -- found second external vertex
                     Finished := True;
                  else
                     -- found first external vertex
                     State (E).Change := True;
                     if Contains (Edge_Pair, (Key (State(E).Ec).Coordinate,
                                  State(E).Direction)) then
                        State (E).Ec_Next :=
                          Find (Edge_Pair, (Key (State(E).Ec).Coordinate,
                                State(E).Direction));
                        if Next (Edge_Pair, State (E).Ec_Next,
                                 State (E).Direction + D90) /=
                          Edge_Pairs.No_Element then
                           State (E).Direction := @ + D90;
                        elsif Next (Edge_Pair, State (E).Ec_Next,
                                    State (E).Direction - D90) /=
                          Edge_Pairs.No_Element then
                           State (E).Direction := @ - D90;
                        else
                           raise Program_Error with
                             "Could not find new edge direction" &
                             Element (State (E).Ec)'Img;
                        end if; -- Next (Edge_Pair, State (E).Ec_Next ...
                     else
                        raise Program_Error with
                          "Expected next element is facing in edge direction";
                     end if; -- Contains (Edge_Pair, (Key (State(E).Ec) ..
                  end if; -- State (E).Change
               end if; -- State (E).Ec_Next = Edge_Pairs.No_Element
            end loop; -- E in Boolean
            Finished := @ or (State (True).Change and State (False).Change);
            if State (True).Ec_Next /= Edge_Pairs.No_Element and
              State (False).Ec_Next /= Edge_Pairs.No_Element and
              not Finished then
               -- Edge pairs exist
               if Edge_Pair (State (True).Ec_Next) = Dummy and
                 Edge_Pair (State (False).Ec_Next) = Dummy then
                  -- not already matched
                  State (True).Ec := State (True).Ec_Next;
                  State (False).Ec := State (False).Ec_Next;
                  Edge_Pair (State (True).Ec) := Key (State (False).Ec);
                  Edge_Pair (State (True).Ec).Facing := @ + D180;
                  Edge_Pair (State (False).Ec) := Key (State (True).Ec);
                  Edge_Pair (State (False).Ec).Facing := @ + D180;
               else
                  Finished := True;
               end if; -- Edge_Pair (State (True).Ec_Next) = Dummy and ...
            else
               Finished := True;
            end if; -- State (True).Ec_Next /= Edge_Pairs.No_Element and ...
         end loop; -- not Finished
      end From_Internal;

      function Unresolved_Vertex (External_Vertex : in Vertex_Sets.Set;
                                  Edge_Pair : in Edge_Pairs.Map)
                                  return Vertex_Sets.Cursor is

         -- Returns the first external vertex with only one edge element that
         -- has not been matched. Returns Vertex_Sets.No_Element if no such
         -- external vertex exists.

         Vc : Vertex_Sets.Cursor := First (External_Vertex);
         Count : Natural := 0;

      begin -- Unresolved_Vertex
         while Count /= 1 and Vc /= Vertex_Sets.No_Element loop
            Count := 0;
            for D in Directions loop
               if Contains (Edge_Pair, (Element (Vc), D)) and then
                 Edge_Pair ((Element (Vc), D)) = Dummy then
                  Count := @ + 1;
               end if; -- Contains (Edge_Pair, (Element (Vc), D)) and then ...
            end loop; -- D in Directions
            Next (Vc);
         end loop; -- Count /= 1 and Vc /= Vertex_Sets.No_Element
         return Vc;
      end Unresolved_Vertex;

      procedure From_External (Vertex : in Coordinates;
                               External_Vertex : in Vertex_Sets.Set;
                               Edge_Pair : in out Edge_Pairs.Map) is

         -- Starting from an external vertex which has only one matched edge,
         -- searches along contigueous matched edges until the first unmatched
         -- edge element is found. This is matched with the unmatched element at
         -- the vertex. Matching continues until an edge element that has
         -- already been matched is found.

         procedure Follow_Edge (State : in out States;
                                Edge : in Boolean;
                                External_Vertex : in Vertex_Sets.Set;
                                Edge_Pair : in out Edge_Pairs.Map) is

            -- Follows edge one step at a time irrespective of changes in
            -- direction.

         begin -- Follow_Edge
            if Next (Edge_Pair, State (Edge).Ec, State (Edge).Direction) /=
              Edge_Pairs.No_Element then
               State (Edge).Ec_Next :=
                 Next (Edge_Pair, State (Edge).Ec, State (Edge).Direction);
            elsif
              Contains (External_Vertex, Key (State (Edge).Ec).Coordinate) then
               -- rotate search at an external vertex
               State (Edge).Ec_Next :=
                 Find (Edge_Pair, (Key (State(Edge).Ec).Coordinate,
                       State(Edge).Direction));
               if Next (Edge_Pair, State (Edge).Ec_Next,
                        State (Edge).Direction + D90) /=
                 Edge_Pairs.No_Element then
                  State (Edge).Direction := @ + D90;
               elsif Next (Edge_Pair, State (Edge).Ec_Next,
                           State (Edge).Direction - D90) /=
                 Edge_Pairs.No_Element then
                  State (Edge).Direction := @ - D90;
               else
                  raise Program_Error with
                    "Could not find new edge direction from external vertex" &
                    Element (State (Edge).Ec)'Img;
               end if; -- Next (Edge_Pair, State (Edge).Ec_Next ...
            else
               -- Internal vertex step diagonally
               if Contains (Edge_Pair,
                            ((Key (State (Edge).Ec).Coordinate.X + 1,
                             Key (State (Edge).Ec).Coordinate.Y + 1),
                             State (Edge).Direction + D180)) then
                  State (Edge).Ec_Next :=
                    Find (Edge_Pair,
                          ((Key (State (Edge).Ec).Coordinate.X + 1,
                           Key (State (Edge).Ec).Coordinate.Y + 1),
                           State (Edge).Direction + D180));
               elsif Contains (Edge_Pair,
                               ((Key (State (Edge).Ec).Coordinate.X + 1,
                                Key (State (Edge).Ec).Coordinate.Y - 1),
                                State (Edge).Direction + D180)) then
                  State (Edge).Ec_Next :=
                    Find (Edge_Pair,
                          ((Key (State (Edge).Ec).Coordinate.X + 1,
                           Key (State (Edge).Ec).Coordinate.Y - 1),
                           State (Edge).Direction + D180));
               elsif Contains (Edge_Pair,
                               ((Key (State (Edge).Ec).Coordinate.X - 1,
                                Key (State (Edge).Ec).Coordinate.Y + 1),
                                State (Edge).Direction + D180)) then
                  State (Edge).Ec_Next :=
                    Find (Edge_Pair,
                          ((Key (State (Edge).Ec).Coordinate.X - 1,
                           Key (State (Edge).Ec).Coordinate.Y + 1),
                           State (Edge).Direction + D180));
               elsif Contains (Edge_Pair,
                               ((Key (State (Edge).Ec).Coordinate.X - 1,
                                Key (State (Edge).Ec).Coordinate.Y - 1),
                                State (Edge).Direction + D180)) then
                  State (Edge).Ec_Next :=
                    Find (Edge_Pair,
                          ((Key (State (Edge).Ec).Coordinate.X - 1,
                           Key (State (Edge).Ec).Coordinate.Y - 1),
                           State (Edge).Direction + D180));
               end if; -- Contains (Edge_Pair ...
               if Next (Edge_Pair, State (Edge).Ec_Next,
                        State (Edge).Direction + D90) /=
                 Edge_Pairs.No_Element then
                  State (Edge).Direction := @ + D90;
               elsif Next (Edge_Pair, State (Edge).Ec_Next,
                           State (Edge).Direction - D90) /=
                 Edge_Pairs.No_Element then
                  State (Edge).Direction := @ - D90;
               else
                  raise Program_Error with
                    "Could not find new edge direction from internal vertex" &
                    Element (State (Edge).Ec)'Img;
               end if; -- Next (Edge_Pair, State (Edge).Ec_Next ...
            end if; -- Next (Edge_Pair, State (Edge).Ec, State (Edge) ...
            State (Edge).Ec := State (Edge).Ec_Next;
         end Follow_Edge;

         State : States;

      begin -- From_External
         -- Find matched and unmatched edge elements at vertex, State (False) is
         -- already matched and State (True) is unmatched.
         for D in Directions loop
            if Contains (Edge_Pair, (Vertex, D)) then
               if Edge_Pair ((Vertex, D)) = Dummy then
                  State (True).Ec := Find (Edge_Pair, (Vertex, D));
                  State (True).Direction := D;
               else
                  State (False).Ec := Find (Edge_Pair, (Vertex, D));
                  State (False).Direction := D;
               end if; -- Edge_Pair ((Vertex, D)) = Dummy
            end if; -- Contains (Edge_Pair, (Vertex, D)) then
         end loop; -- D in Directions
         -- Find initial search directions
         for E in Boolean loop
            if Next (Edge_Pair, State (E).Ec, State (E).Direction - D90) /=
              Edge_Pairs.No_Element then
               State (E).Direction := @ - D90;
            elsif Next (Edge_Pair, State (E).Ec, State (e).Direction + D90)
              /= Edge_Pairs.No_Element then
               State (E).Direction := @ + D90;
            else
               raise Program_Error with "Search direction not found, vertex " &
                 Vertex'Img & " edge " & E'Img;
            end if; -- Next (Edge_Pair, Element (State (E).Ec).Facing - D90) /=
         end loop; -- E in Boolean
         -- Search along matched edges until first unmatched element found.
         while Element (State (False).Ec) /= Dummy loop
            Follow_Edge (State, False, External_Vertex, Edge_Pair);
         end loop; -- Element (State (False).Ec) /= Dummy
         -- Match previously unmatched edge elements
         while Element (State (True).Ec) = Dummy and
           Element (State (False).Ec) = Dummy loop
            Edge_Pair (State (True).Ec) := Key (State (False).Ec);
            Edge_Pair (State (True).Ec).Facing := @ + D180;
            Edge_Pair (State (False).Ec) := Key (State (True).Ec);
            Edge_Pair (State (False).Ec).Facing := @ + D180;
            Follow_Edge (State, True, External_Vertex, Edge_Pair);
            Follow_Edge (State, False, External_Vertex, Edge_Pair);
         end loop; -- Element (State (True).Ec) = Dummy and ...
      end From_External;

   begin -- Cube_Match
      for V in Iterate (Internal_Vertex) loop
         From_Internal (Element (V), Edge_Pair);
      end loop; -- V in Iterate (Internal_Vertex)
      while Unresolved_Vertex (External_Vertex, Edge_Pair) /=
        Vertex_Sets.No_Element loop
         From_External (Element (Unresolved_Vertex (External_Vertex,
                        Edge_Pair)),
                        External_Vertex, Edge_Pair);
      end loop; -- Unresolved_Vertex (External_Vertex, Edge_Pair) =/ ...
   end Cube_Match;

   Jungle_Map : Jungle_Maps.Map;
   Instruction_List : Instruction_Lists.Vector;
   Edge_Pair : Edge_Pairs.Map;
   Internal_Vertex, External_Vertex : Vertex_Sets.Set;

begin -- December_22
   Read_Input (Jungle_Map, Instruction_List);
   Find_Edges (Jungle_Map, Edge_Pair, Internal_Vertex, External_Vertex);
   Wrap_Match (Jungle_Map, Edge_Pair);
   Put_Line ("Part one:" &
               Walk_Path (Jungle_Map, Instruction_List, Edge_Pair)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   -- Reset Edge_Pair for part two
   Find_Edges (Jungle_Map, Edge_Pair, Internal_Vertex, External_Vertex);
   Cube_Match (Internal_Vertex, External_Vertex, Edge_Pair);
   Put_Line ("Part two:" &
               Walk_Path (Jungle_Map, Instruction_List, Edge_Pair)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
