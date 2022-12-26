with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_24 is

   subtype Ordinates is Positive;

   subtype Distances is Natural;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "=" (Left, Right : Coordinates) return Boolean is
     (Left.X = Right.X and Left.Y = Right.Y);

   package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Coordinate_Sets;

   type Directions is (North, South, West, East);

   type Blizzards is array (Directions) of Coordinate_Sets.Set;

   subtype Times is Natural;

   package Blizzard_Stores is new Ada.Containers.Vectors (Times, Blizzards);
   use Blizzard_Stores;

   type State_Element is record
      Time : Times;
      Position : Coordinates;
   end record; -- State_Element

   function "<" (Left, Right : State_Element) return Boolean is
     (Left.Time < Right.Time or
        (Left.Time = Right.Time and Left.Position < Right.Position));

   function "=" (Left, Right : State_Element) return Boolean is
     (Left.Time = Right.Time and Left.Position = Right.Position);

   package State_Sets is new Ada.Containers.Ordered_Sets (State_Element);
   use State_Sets;

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (State_Element);

   package Path_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
   use Path_Queues;

   procedure Read_Input (Wall : out Coordinate_Sets.Set;
                         Blizzard_Store : out Blizzard_Stores.Vector;
                         Start, Finish : out Coordinates) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Ordinates := 1;
      Blizzard : Blizzards := (others => Coordinate_Sets.Empty_Set);

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Wall);
      Clear (Blizzard_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in Positive range 1 .. Length (Text) loop
            case Element (Text, X) is
               when '#' =>
                  Include (Wall, (X, Y));
               when '^' =>
                  Include (Blizzard (South), (X, Y));
               when 'v' =>
                  Include (Blizzard (North), (X, Y));
               when '<' =>
                  Include (Blizzard (West), (X, Y));
               when '>' =>
                  Include (Blizzard (East), (X, Y));
               when '.' =>
                  if Y = 1 then
                     Start := (X, Y);
                  elsif End_Of_File (Input_File) then
                     Finish := (X, Y);
                  end if; -- Y = 1
               when others =>
                  raise Data_Error with "unexpected character '"  &
                    Element (Text, X) & "'";
            end case; -- Element (Text, X)
         end loop; --  X in Positive range 1 .. Length (Text)
         Y := Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Append (Blizzard_Store, Blizzard);
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Find_Path (Wall : in Coordinate_Sets.Set;
                       Blizzard_Store : in out Blizzard_Stores.Vector;
                       Start : in State_Element;
                       Finish :  in Coordinates) return Times is

      subtype Deltas is Integer range -1 .. 1;

      function is_Clear (Blizzard_Store : in out Blizzard_Stores.Vector;
                         Wall : in Coordinate_Sets.Set;
                         Position : in Coordinates; Time : in Times)
                         return Boolean is

         procedure Update (Blizzard_Store : in out Blizzard_Stores.Vector;
                           Wall : in Coordinate_Sets.Set ) is

            -- N.B. Blizzards are named by where the blow from!!!

            type Restarts is array (Directions) of Ordinates;

            Current_Blizzard : constant Blizzards :=
              Last_Element (Blizzard_Store);
            Restart : constant Restarts := (North => First_Element (Wall).Y + 1,
                                            South => Last_Element (Wall).Y - 1,
                                            East => First_Element (Wall).X + 1,
                                            West => Last_Element (Wall).X - 1);
            Next_Blizzard : Blizzards := (others => Coordinate_Sets.Empty_Set);
            Test : Coordinates;

         begin -- Update
            for D in Directions loop
               for I in Iterate (Current_Blizzard (D)) loop
                  Test := Element (I);
                  case D is
                  when North =>
                     Test.Y := Test.Y + 1;
                  when South =>
                     Test.Y := Test.Y - 1;
                  when East =>
                     Test.X := Test.X + 1;
                  when West =>
                     Test.X := Test.X - 1;
                  end case; -- D
                  if Contains (Wall, Test) then
                     -- restart at opposite wall
                     Case D is
                     when North | South =>
                        Test.Y := Restart (D);
                     when East | West =>
                        Test.X := Restart (D);
                     end case; -- D
                     Include (Next_Blizzard (D), Test);
                  else
                     -- continue in same direction
                     Include (Next_Blizzard (D), Test);
                  end if; -- Contains (Wall, Test)
               end loop; -- I in Iterate (Current_Blizzard (D))
            end loop; -- D in Directions
            Append (Blizzard_Store, Next_Blizzard);
         end Update;

         pragma Inline_Always (Update);

      begin -- is_Clear
         if Time > Last_Index (Blizzard_Store) then
            Update (Blizzard_Store, Wall);
         end if; -- Time > Last_Index (Blizzard_Store)
         return not (Contains (Blizzard_Store (Time) (North), Position) or
                       Contains (Blizzard_Store (Time) (South), Position) or
                         Contains (Blizzard_Store (Time) (East), Position) or
                       Contains (Blizzard_Store (Time) (West), Position) or
                         Contains (Wall, (Position)));
      end is_Clear;

      pragma Inline_Always (is_Clear);

      function Next_Step (Current_Element : in State_Element;
                          Blizzard_Store : in out Blizzard_Stores.Vector;
                          Wall : in Coordinate_Sets.Set;
                          Finish : in Coordinates;
                          State_Set : in out State_Sets.Set;
                          Xd, Yd : in Deltas;
                          Next_Element : out State_Element) return Boolean is

         Is_Valid : Boolean;
         X : Integer := Current_Element.Position.X + Xd;
         Y : Integer := Current_Element.Position.Y + Yd;

      begin -- Next_Step
         Is_Valid := (X in Ordinates and Y in Ordinates) and then
           not Contains (State_Set, (Current_Element.Time + 1, (X, Y))) and then
           Is_Clear (Blizzard_Store, Wall, (X, Y), Current_Element.Time + 1);
         if Is_Valid then
            Next_Element.Time := Current_Element.Time + 1;
            Next_Element.Position := (X, Y);
            Include (State_Set, Next_Element);
         end if; -- Is_Valid
         return Is_Valid;
      end Next_Step;

      Current_Element, Next_element : State_Element;
      Path_Queue : Path_Queues.Queue;
      State_Set : State_Sets.Set := State_Sets.Empty_Set;

      pragma Inline_Always (Next_Step);

   begin -- Find_Path
      Current_Element := Start;
      Path_Queue.Enqueue (Current_Element);
      while Path_Queue.Current_Use > 0 and
        Current_Element.Position /= Finish loop
         Path_Queue.Dequeue (Current_Element);
         -- Put (Wall, Blizzard_Store, Current_Element);
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, State_Set,
                       1, 0, Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- East
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, State_Set,
                       0, 1, Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- South
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, State_Set,
                       -1, 0, Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- West
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, State_Set,
                       0, -1, Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- North
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, State_Set,
                       0, 0, Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- Stay put
      end loop;
      if Current_Element.Position = Finish then
         return Current_Element.Time;
      else
         return Times'Last;
      end if; -- Current_Element.Position = Finish
   end Find_Path; -- Find_Path;

   Wall : Coordinate_Sets.Set;
   Blizzard_Store : Blizzard_Stores.Vector;
   Start, Finish : Coordinates;
   Start_State : State_Element;
   End_Time : Times;

begin -- December_24
   Read_Input (Wall, Blizzard_Store, Start, Finish);
   Start_State := (Time => 0, Position => Start);
   End_Time := Find_Path (Wall, Blizzard_Store, Start_State, Finish);
   Put_Line ("Part one:" & End_Time'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Start_State := (Time => End_Time, Position => Finish);
   End_Time := Find_Path (Wall, Blizzard_Store, Start_State, Start);
   Start_State := (Time => End_Time, Position => Start);
   End_Time := Find_Path (Wall, Blizzard_Store, Start_State, Finish);
   Put_Line ("Part two:" & End_Time'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
