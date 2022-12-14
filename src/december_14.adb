with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_14 is

   type Contents is (Air, Rock, Sand);

   type Coordinates is record
      X, Y : Natural;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Grids is new Ada.Containers.Ordered_Maps (Coordinates, Contents);
   use Grids;

   procedure Read_Input (Grid : out Grids.Map) is

      procedure Draw_Line (Grid : in out Grids.Map;
                           Line_Start, Line_End : in Coordinates) is

      begin -- Draw_Line
         if Line_Start.X = Line_End.X then
            -- Vertical line
            if Line_Start.Y <= Line_End.Y then
               for Y in Positive range Line_Start.Y .. Line_End.Y loop
                  Include (Grid, (Line_Start.X, Y), Rock);
               end loop; -- Y in Positive range Line_Start.Y .. Line_End.Y
            else
               for Y in Positive range Line_End.Y .. Line_Start.Y loop
                  Include (Grid, (Line_Start.X, Y), Rock);
               end loop; -- Y in Positive range Line_End.Y .. Line_Start.Y
            end if; -- Line_Start.Y <= Line_End.Y
         elsif Line_Start.Y = Line_End.Y then
            -- Horizontal Line
            if Line_Start.X <= Line_End.X then
               for X in Positive range Line_Start.X .. Line_End.X loop
                  Include (Grid, (X, Line_Start.Y), Rock);
               end loop; -- X in Positive range Line_Start.X .. Line_End.X
            else
               for X in Positive range Line_End.X .. Line_Start.X loop
                  Include (Grid, (X, Line_Start.Y), Rock);
               end loop; -- X in Positive range Line_End.X .. Line_Start.X
            end if; -- Line_Start.Y <= Line_End.Y
         else
            raise Constraint_Error with
              "Only horizontal or vertical lines permitted";
         end if; -- Line_Start.X = Line_End.X
      end Draw_Line;

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Line_Start, Line_End : Coordinates;
      X0 : Natural := Natural'Last;
      X1, Y1 : Natural := Natural'First;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_14.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Grid);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Line_Start.X := Natural'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Line_Start.Y := Natural'Value (Slice (Text, First, Last));
         if Line_Start.X < X0 then
            X0 := Line_Start.X;
         end if; -- Line_Start.X < X0
         if X1 < Line_Start.X then
            X1 := Line_Start.X;
         end if; -- < Line_Start.x
         if Y1 < Line_Start.Y then
            Y1 := Line_Start.Y;
         end if; -- Y1 < Line_Start.Y
         Start_At := Last + 1;
         while Start_At < Length (Text) loop
            Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
            Line_End.X := Natural'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
            Line_End.Y := Natural'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
            Draw_Line (Grid, Line_Start, Line_End);
            if Line_End.X < X0 then
               X0 := Line_End.X;
            end if; -- Line_End.X < X0
            if X1 < Line_End.X then
               X1 := Line_Start.X;
            end if; -- < Line_End.x
            if Y1 < Line_End.Y then
               Y1 := Line_Start.Y;
            end if; -- Y1 < Line_End.Y
            Line_Start := Line_End;
         end loop;
      end loop; -- not End_Of_File (Input_File)
      -- Fill Grid with air
      for Y in Natural range 0 .. Y1 loop
         for X in Natural range X0 .. X1 loop
            if not Contains (Grid, (X, Y)) then
               Include (Grid, (X, Y), Air);
            end if; -- not Contains (Grid, (X, Y))
         end loop; -- X in Natural range X0 .. X1
      end loop; -- in Natural range 0 .. Y1
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Flow (Grid : in out Grids.Map;
                  Source : in Coordinates) return Natural is

      function Step (Grid : in out Grids.Map;
                     Start : in Coordinates) return Coordinates is

         -- Returns original position if stable or a new position if still
         -- capable of moving. Result may be out of map.

         Result : Coordinates := Start;

      begin -- Step
         Result.Y := Start.Y + 1; -- try down
         if Contains (Grid, Result) then
            case Grid (Result) is
               when Air =>
                  null; -- straight down
               when Rock | Sand =>
                  Result.X := Start.X - 1; -- try down to left
                  If not Contains (Grid, Result) or else
                    Grid (Result) = Air then
                     null; -- down to left
                  else
                     Result.X := Start.X + 1; -- try down to right
                     If not Contains (Grid, Result) or else
                       Grid (Result) = Air then
                        null; -- down to right
                     else
                        Result := Start; -- stopped stable on sand
                     end if; -- not Contains (Grid, Result) or else
                  end if; -- If not Contains (Grid, Result) or else
            end case;
         end if; -- Contains (Grid, Result)
         return Result;
      end Step;

      Sand_Count : Natural := 0;
      New_Position, Current_Position : Coordinates;

   begin -- Flow
      loop -- Sand units
         Current_Position := Source;
         loop -- moving
            New_Position := Step (Grid, Current_Position);
            exit when New_Position = Current_Position or
              not Contains (Grid, New_Position);
            Current_Position := New_Position;
         end loop; -- moving
         if Contains (Grid, New_Position) and -- part 1 termination
           not (New_Position = Source) then -- part 2 termination
            Grid (New_Position) := Sand;
            Sand_Count := Sand_Count + 1;
         else
            exit; -- Sand left map (part one) or jammed source (part 2)
         end if; -- Contains (Grid, New_Position) and ...
      end loop; -- Sand units
      return Sand_Count;
   end Flow;

   procedure Add_Floor (Grid : in out Grids.Map;
                        Source : in Coordinates) is

      Y_Floor : constant Natural := Last_Key (Grid).Y + 2;
      X_Limit : constant Natural := 2 * Source.X + 1;

   begin --
      -- Floor
      for X in Natural range 0 .. X_Limit loop
         Include (Grid, (X, Y_Floor), Rock);
      end loop; -- X in Natural range 0 .. X_Limit
      -- Air
      for Y in Natural Range 0 .. Y_Floor loop
         for X in Natural range 0 .. X_Limit loop
            if not Contains (Grid, (X, Y)) then
               Include (Grid, (X, Y), Air);
            end if; -- not Contains (Grid, (X, Y))
         end loop; -- X in Natural range 0 .. X_Limit
      end loop; -- Y in Natural Range 0 .. Y_Floor
   end;

   Source : Coordinates := (500, 0);
   Grid : Grids.Map;

begin -- December_14
   Read_Input (Grid);
   Put_Line ("Part one:" & Flow (Grid, Source)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Grid);
   Add_Floor (Grid, Source);
   -- N.B. the + 1 is to allow for the one unit of sand stuck at the source that
   -- did not move!
   Put_Line ("Part two:" & Natural'Image (Flow (Grid, Source) + 1));
   DJH.Execution_Time.Put_CPU_Time;
end December_14;
