with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

-- Getting closer, criteria for establishing a cycle works for example but not
-- for my input. The real criteria for a repeat id the same rock index, same gas
-- index and the same state of the top four layers of the tower. Four layers
-- because the tallest rock is four units high

procedure December_17 is

   subtype X_Ordinates is Natural range 0 .. 6;

   subtype X_R_Ordinates is X_Ordinates range 0 .. 3;

   subtype Y_Ordinates is Natural;

   subtype Y_R_Ordinates is Y_Ordinates range 0 .. 3;

   X_Start : constant X_Ordinates := 2;
   Y_Start : constant Y_Ordinates := 3;

   type Rocks is array (X_R_Ordinates, Y_R_Ordinates) of Boolean;

   type Rock_Indices is mod 5;

   type Rock_Arrays is array (Rock_Indices) of Rocks;

   type Coordinates is record
      X : X_Ordinates;
      Y : Y_Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
      (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "=" (Left, Right : Coordinates) return Boolean is
      (Left.X = Right.X and Left.Y = Right.Y);

   package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Coordinate_Sets;

   subtype Gas_Indices is Positive;

   package Gas_Index_Sets is new Ada.Containers.Ordered_Sets (Gas_Indices);
   use  Gas_Index_Sets;

   type Rock_Counts is record
      Rock_Count, Rock_Height : Natural;
   end record; -- Rock_Counts

   package Rock_Count_Maps is new
     Ada.Containers.Ordered_Maps (Gas_Indices, Rock_Counts);

   type Cycle_Pairs is record
      Gas_Index_Set : Gas_Index_Sets.Set := Gas_Index_Sets.Empty_Set;
      Rock_Count_Map : Rock_Count_Maps.Map := Rock_Count_Maps.Empty_Map;
   end record; -- Cycle_Pairs

   procedure Read_Input (Gas_Jet_Data : out Unbounded_String;
                         Rock_Data : out Rock_Arrays) is

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_17.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Gas_Jet_Data);
      Close (Input_File);
      Open (Input_File, In_File, "december_17_rocks.txt");
      for R in Rock_Indices loop
         for Y in reverse Y_R_Ordinates loop
            Get_Line (Input_File, Text);
            for X in X_R_Ordinates loop
               Rock_Data (R) (X, Y) := Element (Text, X + 1) = '#';
            end loop; -- X in X_R_Ordinates
         end loop; -- Y in reverse Y_R_Ordinates
         Skip_Line (Input_File);
      end loop; -- R in Rock_Indices
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Put (Chamber : in Coordinate_Sets.Set) is

   begin -- Put
      for Y in reverse Y_Ordinates range 0 .. Last_Element (Chamber).Y loop
         Put ('|');
         for X in X_Ordinates loop
            if Contains (Chamber, (X, Y)) then
               Put ('#');
            else
               Put ('.');
            end if; --
         end loop; --
         Put_Line ("|");
      end loop; -- Y in reverse Y_Ordinates range 0 .. Last_Element (Chamber).Y
      Put_Line ("+-------+");
      New_Line;
   end; --Put

   procedure Fall (Chamber : in out Coordinate_Sets.Set;
                   Rock : in Rocks;
                   Origin_In : in Coordinates;
                   Gas_Jet_Data : in Unbounded_String;
                   Gas_Index : in out Gas_Indices) is

      procedure Set_Rock (Chamber : in out Coordinate_Sets.Set;
                          Rock : in Rocks;
                          Origin : in Coordinates) is
      begin -- Set_Rock
         for X in X_R_Ordinates loop
            for Y in Y_R_Ordinates loop
               if Rock (X, Y) then
                  include (Chamber, (X + Origin.X, Y + Origin.Y));
               end if; -- Rock (X, Y)
            end loop; -- Y in Y_R_Ordinates
         end loop; -- X in X_R_Ordinates
      end Set_Rock;

      Origin : Coordinates := Origin_In;
      Is_Resting : Boolean := False;
      Jet : Integer;
      Test : Coordinates;
      Jet_Effective : Boolean;

   begin -- Fall
      loop -- Falling
         case Element (Gas_Jet_Data, Gas_Index) is
            when '<' =>
               Jet := -1;
            when '>' =>
               Jet := 1;
            when others =>
               raise Constraint_Error with "Unexpected character '" &
                 Element (Gas_Jet_Data, Gas_Index);
         end case; -- Element (Gas_Jet_Data, Gas_Index)
         -- Horizontal movement
         Jet_Effective := True;
         for X in X_R_Ordinates loop
            for Y in Y_R_Ordinates loop
               if Rock (X, Y) then
                  -- Test for horizontal interference with walls
                  if Integer (Origin.X + Jet + X) in X_Ordinates then
                     Test.X := Origin.X + Jet + X;
                     Test.Y := Origin.Y + Y;
                     -- Test horizontal inteference with rock
                     Jet_Effective := Jet_Effective
                       and not Contains (Chamber, Test);
                  else
                     Jet_Effective := False;
                  end if; -- Integer (Origin.X + Jet + X) in X_Ordinates
               end if; -- Rock (X, Y)
            end loop; -- Y in Y_R_Ordinates
         end loop; -- X in X_R_Ordinates
         if Jet_Effective then
            Origin.X := Origin.X + Jet;
         end if; -- not Jet_Effective
         Gas_Index := Gas_Index + 1;
         if Gas_Index > Length (Gas_Jet_Data) then
            Gas_Index := 1;
         end if; -- Gas_Index > Length (Gas_Jet_Data)
         for X in X_R_Ordinates loop
            for Y in Y_R_Ordinates loop
               if Rock (X, Y) then
                  Test.X := Origin.X + X;
                  if Integer (Origin.Y - 1 + Y) in Y_Ordinates then
                     Test.Y := Origin.Y - 1 + Y;
                     Is_Resting := Is_Resting or Contains (Chamber, Test);
                  else
                     -- Reached rock bottom
                     IS_Resting := True;
                  end if; -- Integer (Origin.Y - 1 + Y) in Y_Ordinates
               end if; -- Rock (X, Y)
            end loop; -- Y in Y_R_Ordinates
         end loop; -- X in X_R_Ordinates
         exit when Is_Resting;
         Origin.Y := Origin.Y - 1;
      end loop; -- Falling
      Set_Rock (Chamber, Rock, Origin);
   end Fall;

   function Pile (Gas_Jet_Data : in Unbounded_String;
                  Rock_Data : in Rock_Arrays) return Natural is

      Origin : Coordinates := (X_Start, Y_Start);
      Chamber : Coordinate_Sets.Set;
      Gas_Index : Gas_Indices := 1;
      Ri : Rock_Indices := Rock_Indices'First;

   begin -- Pile
      Clear (Chamber);
      for Rc in Positive range 1 .. 2022 loop
         Fall (Chamber, Rock_Data (Ri), Origin, Gas_Jet_Data, Gas_Index);
         Origin.Y := Last_Element (Chamber).Y + Y_Start + 1;
         Ri := Ri + 1;
      end loop; -- R in Rock_Indices
      return Last_Element (Chamber).Y + 1;
   end Pile;

   function Big_Pile (Gas_Jet_Data : in Unbounded_String;
                      Rock_Data : in Rock_Arrays) return Unsigned_64 is

      --  function Top_Row (Chamber : in Coordinate_Sets.Set) Return Unsigned_8 is
      --
      --     Result : Unsigned_8 := 0;
      --     MasK : constant Unsigned_8 := 2#00000001#;
      --
      --  begin -- Top_Row
      --     for X in X_Ordinates loop
      --        if Contains (Chamber, (X, Last_Element (Chamber).Y)) then
      --           Result := Result or Shift_Left (Mask,X_Ordinates'Last - X);
      --        end if; -- Contains (Chamber, (X, Last_Element (Chamber).X))
      --     end loop; -- X in reverse X_R_Ordinates
      --     return Result;
      --  end Top_Row;

      Total_Rocks : constant Unsigned_64 := 1000000000000;
      Origin : Coordinates := (X_Start, Y_Start);
      Chamber : Coordinate_Sets.Set;
      Gas_Index : Gas_Indices := 1;
      Rock_Count : Natural := 0;
      Ri : Rock_Indices;
      Cycle : array (Rock_Indices) of Cycle_Pairs;
      Cycle_Height, Remaining_Rocks, Remaining_Cycles, Modulus : Unsigned_64;

   begin -- Big_Pile
      Clear (Chamber);
      Ri := Rock_Indices (Rock_Count mod Rock_Indices'Modulus);
      Rock_Count := Rock_Count + 1;
      Fall (Chamber, Rock_Data (Ri), Origin, Gas_Jet_Data, Gas_Index);
      Origin.Y := Last_Element (Chamber).Y + Y_Start + 1;
      loop -- Until repeated pattern
         Ri := Rock_Indices (Rock_Count mod Rock_Indices'Modulus);
         Rock_Count := Rock_Count + 1;
         Fall (Chamber, Rock_Data (Ri), Origin, Gas_Jet_Data, Gas_Index);
         Origin.Y := Last_Element (Chamber).Y + Y_Start + 1;
         if Contains (Cycle (RI).Gas_Index_Set, Gas_Index) then
            exit;
         else
            Include (Cycle (RI).Gas_Index_Set, Gas_Index);
            Rock_Count_Maps.Include (Cycle (RI).Rock_Count_Map, Gas_Index,
                                     (Rock_Count, Last_Element (Chamber).Y + 1));
         end if; -- RI = 0 and Gas_Index = 1
      end loop; -- Until repeated pattern
      Put_Line ("Rock_Count:" & Rock_Count'Img & Cycle (RI).Rock_Count_Map (Gas_Index).Rock_Count'Img &
                  " Height:" & Positive'Image (Last_Element (Chamber).Y + 1) & Cycle (RI).Rock_Count_Map (Gas_Index).Rock_Height'Img);
      Cycle_Height :=
        Unsigned_64 (Last_Element (Chamber).Y + 1 -
                         Cycle (RI).Rock_Count_Map (Gas_Index).Rock_Height);
      Modulus :=
        Unsigned_64 (Rock_Count -
                       Cycle (RI).Rock_Count_Map (Gas_Index).Rock_Count);
      Remaining_Rocks := Total_Rocks - Unsigned_64 (Rock_Count);
      Remaining_Cycles := Remaining_Rocks / Modulus;
      Remaining_Rocks := Remaining_Rocks - Remaining_Cycles * Modulus;
      Put_Line ("Modulus:" & Modulus'Img & " Cycle_Height:" & Cycle_Height'Img &
                  " Remaining_Cycles:" & Remaining_Cycles'Img & " Remaining_Rocks:" & Remaining_Rocks'Img);
      if Remaining_Rocks > 0 then
         Remaining_Rocks := Remaining_Rocks + Unsigned_64 (Rock_Count);
         -- required because of post tested loop
         loop -- Until Remaining_Rocks
            Ri := Rock_Indices (Rock_Count mod Rock_Indices'Modulus);
            Rock_Count := Rock_Count + 1;
            Fall (Chamber, Rock_Data (Ri), Origin, Gas_Jet_Data, Gas_Index);
            Origin.Y := Last_Element (Chamber).Y + Y_Start + 1;
            exit when Unsigned_64 (Rock_Count) = Remaining_Rocks;
         end loop; -- Until Remaining_Rocks
      end if; -- Remaining_Rocks > 0
      return Remaining_Cycles * Cycle_Height +
        Unsigned_64 (Last_Element (Chamber).Y + 1);
   end Big_Pile;

   Gas_Data : Unbounded_String;
   Rock_Data : Rock_Arrays;

begin -- December_17
   Read_Input (Gas_Data, Rock_Data);
   Put_Line ("Part one:" & Pile (Gas_Data, Rock_Data)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Big_Pile (Gas_Data, Rock_Data)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_17;
