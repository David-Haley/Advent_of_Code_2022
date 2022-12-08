with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_08 is

   subtype Heights is Natural range 0 .. 9;
   subtype Test_Heights is Integer range Heights'First - 1 .. Heights'Last;

   type Trees is record
      Height : Heights;
      Visible : Boolean := False;
   end record; -- Trees;

   Type Coordinates is record
      X, Y : Positive;
   end record; -- Coordinates;

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or
        (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Forests is new Ada.Containers.Ordered_Maps (Coordinates, Trees);
   use Forests;

   procedure Read_Input (Forest : out Forests.Map)is

      Input_File : File_Type;
      Text : Unbounded_String;
      Y : Positive := 1;
      Tree : Trees;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_08.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Forest);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for X in 1 .. Length (Text) loop
            Tree.Height := Heights'Value (Slice (Text, X, X));
            include (Forest, (X, Y), Tree);
         end loop;
         Y := Y + 1;
      end loop; -- not End_Of_File (Input_File) and Not_Finished
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Sight_Lines (Forest : in out Forests.Map;
                         X_Limit, Y_Limit : in Positive) is

      Test_Height : Test_Heights;
      Coordinate : Coordinates;

   begin -- Sight_Lines
      -- look from Left
      for Y in Positive range 1 .. Y_Limit loop
         Test_Height := Test_Heights'First;
         Coordinate.Y := Y;
         for X in Positive range 1 .. X_Limit loop
            Coordinate.X := X;
            Forest (Coordinate).Visible := Forest (Coordinate).Visible or
              Forest (Coordinate).Height > Test_Height;
            if Forest (Coordinate).Height > Test_Height then
               Test_Height := Forest (Coordinate).Height;
            end if; -- Forest (Coordinate).Height > Test_Height
         end loop; -- X in Positive range 1 .. X_Limit
      end loop; -- Y in Positive range 1 .. Y_Limit
      -- look from Right
      for Y in Positive range 1 .. Y_Limit loop
         Test_Height := Test_Heights'First;
         Coordinate.Y := Y;
         for X in reverse Positive range 1 .. X_Limit loop
            Coordinate.X := X;
            Forest (Coordinate).Visible := Forest (Coordinate).Visible or
              Forest (Coordinate).Height > Test_Height;
            if Forest (Coordinate).Height > Test_Height then
               Test_Height := Forest (Coordinate).Height;
            end if; -- Forest (Coordinate).Height > Test_Height
         end loop; -- X in reverse Positive range 1 .. X_Limit
      end loop; -- Y in Positive range 1 .. Y_Limit
      -- Look from Top
      for X in Positive range 1 .. X_Limit loop
         Coordinate.X := X;
         Test_Height := Test_Heights'First;
         for Y in Positive range 1 .. Y_Limit loop
            Coordinate.Y := Y;
            Forest (Coordinate).Visible := Forest (Coordinate).Visible or
              Forest (Coordinate).Height > Test_Height;
            if Forest (Coordinate).Height > Test_Height then
               Test_Height := Forest (Coordinate).Height;
            end if; -- Forest (Coordinate).Height > Test_Height
         end loop; -- Y in Positive range 1 .. Y_Limit
      end loop; -- X in Positive range 1 .. X_Limit
      -- Look from Bottom
      for X in Positive range 1 .. X_Limit loop
         Coordinate.X := X;
         Test_Height := Test_Heights'First;
         for Y in reverse Positive range 1 .. Y_Limit loop
            Coordinate.Y := Y;
            Forest (Coordinate).Visible := Forest (Coordinate).Visible or
              Forest (Coordinate).Height > Test_Height;
            if Forest (Coordinate).Height > Test_Height then
               Test_Height := Forest (Coordinate).Height;
            end if; -- Forest (Coordinate).Height > Test_Height
         end loop; -- Y in reverse Positive range 1 .. Y_Limit
      end loop; -- X in Positive range 1 .. X_Limit
   end Sight_Lines;

   function Count_Visible (Forest : in Forests.Map) return Natural is

      Result : Natural := 0;

   begin -- Count_Visible
      for F in Iterate (Forest) loop
         if Forest (F).Visible then
            Result := Result + 1;
         end if; -- Forest (F).Visible
      end loop; -- Forest (F).Visible
      return Result;
   end Count_Visible;

   function Most_Senic (Forest : in Forests.Map;
                        X_Limit, Y_Limit : in  Positive) return Positive is

      function Senic_Score  (Forest : in Forests.Map;
                             X, Y : in Positive;
                             X_Limit, Y_Limit : in  Positive) return Positive is

         -- Calculates the senic score of a tree located at (X, Y).

         View_Left, View_Right, View_Up, View_Down : Positive;
         Coordinate : Coordinates := (X, Y);
         Test_Height : constant Test_Heights := Forest (Coordinate).Height;

      begin -- Senic_Score
         Coordinate := (X - 1, Y);
         loop -- Looking Left
            exit when 1 >= Coordinate.X or
              Test_Height <= Forest (Coordinate).Height;
            Coordinate.X := Coordinate.X - 1;
         end loop; -- Looking Left
         View_Left := X - Coordinate.X;
         Coordinate :=  (X + 1, Y);
         loop -- Looking Right
            exit when X_Limit <= Coordinate.X or
              Test_Height <= Forest (Coordinate).Height;
            Coordinate.X := Coordinate.X + 1;
         end loop; -- Looking Right
         View_Right := Coordinate.X - X;
         Coordinate := (X, Y - 1);
         loop -- Looking Up
            exit when 1 >= Coordinate.Y or
              Test_Height <= Forest (Coordinate).Height;
            Coordinate.Y := Coordinate.Y - 1;
         end loop; -- Looking Up
         View_Up := Y - Coordinate.Y;
         Coordinate := (X, Y + 1);
         loop -- Looking Down
            exit when  Y_Limit <= Coordinate.Y or
              Test_Height <= Forest (Coordinate).Height;
            Coordinate.Y := Coordinate.Y + 1;
         end loop; -- Looking Down
         View_Down := Coordinate.Y - Y;
         return View_Left * View_Right * View_Up * View_Down;
      end Senic_Score;

      Result : Positive := 1;
      Score : Positive;

   begin -- Most_Senic
      for F in Iterate (Forest) loop
         if Key (F).X /= 1 and Key (F).X /= X_Limit and
           Key (F).Y /= 1 and Key (F).Y /= Y_Limit then
            -- Edge trees all have a zero score
            Score := Senic_Score (Forest, Key (F).X, Key (F).Y, X_Limit,
                                  Y_Limit);
            if Score > Result then
               Result := Score;
            end if; -- Score > Result
         end if; -- Key (F).X /= 1 and Key (F).X /= X_Limit and ...
      end loop; -- F in Iterate (Forest)
      return Result;
   end Most_Senic;

   Forest : Forests.Map;
   X_Limit, Y_Limit : Positive;

begin -- December_08
   Read_Input (Forest);
   -- A rectangular array is assumed
   X_Limit := Last_Key (Forest).X;
   Y_Limit := Last_Key (Forest).Y;
   Sight_Lines (Forest, X_Limit, Y_Limit);
   Put_Line ("Part one:" & Count_Visible (Forest)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Most_Senic (Forest, X_Limit, Y_Limit)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_08;
