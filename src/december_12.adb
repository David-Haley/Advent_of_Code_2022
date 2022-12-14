with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_12 is

   subtype Elevations is Character range 'a' .. 'z';

   type Ends is (E, S);

   type Coordinates is record
      X, Y : Natural;
   end record; -- Coordinates

   type End_Points is array (Ends) of Coordinates;

   type Points is record
      Elevation : Elevations;
      Visited : Boolean := False;
   end record; -- Pounts;

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or
        (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Grids is new Ada.Containers.Ordered_Maps (Coordinates, Points);
   use Grids;

   type Queue_Elements is record
      Coordinate : Coordinates;
      Distance : Natural;
   end record; -- Query_Elements

   package Q_Int is new
     Ada.Containers.Synchronized_Queue_Interfaces (Queue_Elements);
   package Point_Qs is new Ada.Containers.Unbounded_Synchronized_Queues (Q_Int);

   procedure Read_Input (Grid : out Grids.Map; End_Point : out End_Points) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Coordinate : Coordinates;
      Point : Points;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_12.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Grid);
      Coordinate.Y := 1;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         For X in Positive range 1 .. Length (Text) loop
            Coordinate.X := X;
            if Element (Text, X) in Elevations then
               Point.Elevation := Element (Text, X);
               Include (Grid, Coordinate, Point);
            else
               End_Point (Ends'Value (Slice (Text, X, X))) := Coordinate;
            end if; -- Element (Text, X) in Elevations
         end loop; -- X in Positive range 1 .. Length (Text)
         Coordinate.Y := Coordinate.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      Point.Elevation := Elevations'Last;
      Include (Grid, End_Point (E), Point);
      Point.Visited := True;
      Point.Elevation := Elevations'First;
      Include (Grid, End_Point (S), Point);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   Function Find_Path (Grid : in out Grids.Map;
                       End_Point : in End_Points) return Natural is

      function Valid_Step (Grid : in Grids.Map;
                           Current_Point, Next_Point : in Queue_Elements)
                           return Boolean is

      begin -- Valid_Step
         return Contains (Grid, Next_Point.Coordinate) and then
           not Grid (Next_Point.Coordinate).Visited and then
           Grid (Next_Point.Coordinate).Elevation <=
           Elevations'Succ (Grid (Current_Point.Coordinate).Elevation);
      end Valid_Step;

      Current_Point, Next_Point : Queue_Elements;
      Point_Q : Point_Qs.Queue;

   begin -- Find_Path
      Current_Point.Coordinate := End_Point (S);
      Current_Point.Distance := 0;
      Point_Q.Enqueue (Current_Point);
      while Point_Q.Current_Use > 0 and Current_Point.Coordinate /=
        End_Point (E) loop
         Point_Q.Dequeue (Current_Point);
         -- Put_Line (Current_Point'Img);
         Next_Point.Distance := Current_Point.Distance + 1;
         if Current_Point.Coordinate /= End_Point (E) then
            -- Search Left
            Next_Point.Coordinate.X := Current_Point.Coordinate.X - 1;
            Next_Point.Coordinate.Y := Current_Point.Coordinate.Y;
            if Valid_Step (Grid, Current_Point, Next_Point) then
               Grid (Next_Point.Coordinate).Visited := True;
               Point_Q.Enqueue (Next_Point);
            end if; -- Valid_Step (Grid, Current_Point, Next_Point)
            -- Search Right
            Next_Point.Coordinate.X := Current_Point.Coordinate.X + 1;
            if Valid_Step (Grid, Current_Point, Next_Point) then
               Grid (Next_Point.Coordinate).Visited := True;
               Point_Q.Enqueue (Next_Point);
            end if; -- Valid_Step (Grid, Current_Point, Next_Point)
            -- Search Up
            Next_Point.Coordinate.X := Current_Point.Coordinate.X;
            Next_Point.Coordinate.Y := Current_Point.Coordinate.Y - 1;
            if Valid_Step (Grid, Current_Point, Next_Point) then
               Grid (Next_Point.Coordinate).Visited := True;
               Point_Q.Enqueue (Next_Point);
            end if; -- Valid_Step (Grid, Current_Point, Next_Point)
            -- Search Down
            Next_Point.Coordinate.Y := Current_Point.Coordinate.Y + 1;
            if Valid_Step (Grid, Current_Point, Next_Point) then
               Grid (Next_Point.Coordinate).Visited := True;
               Point_Q.Enqueue (Next_Point);
            end if; -- Valid_Step (Grid, Current_Point, Next_Point)
         end if; -- Current_Point.Coordinate /= End_Point (E)
      end loop; -- Point_Q.Current_Use > 0 and Current_Point.Coordinate /= ...
      if Current_Point.Coordinate = End_Point (E) then
         return Current_Point.Distance;
      else
         return 0;
      end if; -- Current_Point.Coordinate = End_Point (E)
   end Find_Path;

   function Find_Shortest_Path (Grid : in out Grids.Map;
                                End_Point_In : in End_Points) return Natural is

      Left, Right, Top, Bottom : Natural;
      End_Point : End_Points := End_Point_In;
      Distance : Natural;
      Result : Natural := Natural'Last;

   begin -- Find_Shortest_Path
      -- Find edges
      Left := First_Key (Grid).X;
      Top := First_Key (Grid).Y;
      Right := Last_Key (Grid).X;
      Bottom := Last_Key (Grid).Y;
      for Gs in Iterate (Grid) loop
         if (Key (Gs).X = Left or Key (Gs).X = Right or
               Key (Gs).Y = Top or Key (Gs).Y = Bottom)
           and Grid (Gs).Elevation = Elevations'First then
            -- Outside edge and lowest points
            for G in Iterate (Grid) loop
               Grid (G).Visited := False;
            end loop; -- G in Iterate (Grid)
            Grid (Gs).Visited := True;
            End_Point (S) := Key (Gs);
            Distance := Find_Path (Grid, End_Point);
            if Distance > 0 and Distance < Result then
               Result := Distance;
            end if; -- Distance > 0 and Distance < Result
         end if; -- (Key (Gs).X = Left or Key (Gs).X = Right or ...
      end loop; -- Gs in Iterate (Grid)
      return Result;
   end Find_Shortest_Path;

   Grid : Grids.Map;
   End_Point : End_Points;

begin -- December_12
   Read_Input (Grid, End_Point);
   Put_Line ("Part one:" & Find_Path (Grid, End_Point)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Find_Shortest_Path (Grid, End_Point)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_12;
