with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
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
      Distance : Distances;
   end record; -- State_Element

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (State_Element);

   type Priority is record
      Time : Times;
      Distance : Distances;
   end record; -- Priority

   function Get_Priority (Element : State_Element) return Priority is
     (Element.Time, Element.Distance);

   function Shorter_First (Left, Right : Priority) return Boolean is
     (Left.Time < Right.Time or
        (Left.Time = Right.Time and Left.Distance < Right.Distance));

   package Path_Queues is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Queue_Interface,
      Queue_Priority => Priority,
      Get_Priority => Get_Priority,
      Before => Shorter_First);
   use Path_Queues;
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

   procedure Put (Wall : in Coordinate_Sets.Set;
                  Blizzard_Store : in Blizzard_Stores.Vector;
                  Current_Element : in State_Element) is

      Combined : Coordinate_Sets.Set := Coordinate_Sets.Empty_Set;

   begin -- Put
      Put_Line (Current_Element.Time'img & Current_Element.Distance'Img);
      Combined := Blizzard_Store (Current_Element.Time) (North) or
        Blizzard_Store (Current_Element.Time) (South) or
        Blizzard_Store (Current_Element.Time) (East) or
        Blizzard_Store (Current_Element.Time) (West);
      for Y in Ordinates range 1 .. Last_Element (Wall).Y loop
         for X in Ordinates range 1 .. Last_Element (Wall).X loop
            if Contains (Wall, (X, Y)) then
               Put ('#');
            elsif Contains (Combined, (X, Y)) then
               if (X, Y) = Current_Element.Position then
                  Put ('?');
               else
                  Put ('B');
               end if;
            elsif (X, Y) = Current_Element.Position then
               Put ('E');
            else
               Put ('.');
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Put;

   function Find_Path (Wall : in Coordinate_Sets.Set;
                       Blizzard_Store : in out Blizzard_Stores.Vector;
                       Start, Finish :  in Coordinates) return Times is

      subtype Deltas is Integer range -1 .. 1;

      Function Man_Distance (P1, P2 : in Coordinates) return Distances is
        (abs (P1.X - P2.X) + abs (P1.Y - P2.Y));

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
         while Time > Last_Index (Blizzard_Store) loop
            Update (Blizzard_Store, Wall);
            Put_Line ("Update:" & Time'Img);
         end loop; -- Time > Last_Index (Blizzard_Store)
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
                          Xd, Yd : in Deltas;
                          Next_Element : out State_Element) return Boolean is

         Is_Valid : Boolean;

      begin -- Next_Step
         Is_Valid := (Current_Element.Position.X + Xd in Ordinates and
                        Current_Element.Position.Y + Yd in Ordinates) and then
           Is_Clear (Blizzard_Store, Wall,
                     (Current_Element.Position.X + Xd,
                      Current_Element.Position.Y + Yd),
                     Current_Element.Time + 1);
         if Is_Valid then
            Next_Element.Time := Current_Element.Time + 1;
            Next_Element.Position :=
              (Current_Element.Position.X + Xd, Current_Element.Position.Y + Yd);
            Next_Element.Distance :=
              Man_Distance ((Current_Element.Position.X + Xd,
                            Current_Element.Position.Y + Yd), Finish);
         end if; -- Is_Valid
         return Is_Valid;
      end Next_Step;

      Current_Element, Next_element : State_Element;
      Path_Queue : Path_Queues.Queue;

      pragma Inline_Always (Next_Step);


   begin -- Find_Path
      Current_Element := (Time => 0,
                          Position => Start,
                          Distance => Man_Distance (Finish, Start));
      Path_Queue.Enqueue (Current_Element);
      while Path_Queue.Current_Use > 0 and
        Current_Element.Position /= Finish loop
         Path_Queue.Dequeue (Current_Element);
         -- Put (Wall, Blizzard_Store, Current_Element);
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, 1, 0,
                       Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- East
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, 0, 1,
                       Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- South
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, -1, 0,
                       Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- West
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, 0, -1,
                       Next_Element) then
            Path_Queue.Enqueue (Next_Element);
         end if; -- North
         if Next_Step (Current_Element, Blizzard_Store, Wall, Finish, 0, 0,
                       Next_Element) then
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

begin -- December_24
   Read_Input (Wall, Blizzard_Store, Start, Finish);
   Put_Line ("Part one:" &
               Find_Path (Wall, Blizzard_Store, Start, Finish)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
