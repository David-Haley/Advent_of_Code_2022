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
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_09 is

   type Directions is (U, D, L, R);

   type Moves is Record
      Direction : Directions;
      Distance : Positive;
   end record; -- Moves

   package Move_Stores is new Ada.Containers.Vectors (Positive, Moves);
   use Move_Stores;

   type Coordinates is record
      X, Y : Integer := 0;
   end record; -- Coordinates;

   type Rope_Indices is (Head, Knot_1, Knot_2, Knot_3, Knot_4, Knot_5, Knot_6,
                         Knot_7, Knot_8, Knot_9);

   subtype Knot_Indices is Rope_Indices range Knot_1 .. Knot_9;

   type Ropes is array (Rope_Indices) of Coordinates;

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or
        (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Coordinate_Sets;

   procedure Read_Input (Move_Store : out Move_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Move : Moves;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_09.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Move_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Upper_Set, Start_At, Inside, First, Last);
         Move.Direction := Directions'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Move.Distance := Positive'Value (Slice (Text, First, Last));
         Append (Move_Store, Move);
      end loop; -- not End_Of_File (Input_File) and Not_Finished
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Step_Head (Rope : in out Ropes; Direction : in Directions) is

   begin -- Step_Head
      case Direction is
         when U =>
            Rope (Head).Y := Rope (Head).Y + 1; -- Up
         when D =>
            Rope (Head).Y := Rope (Head).Y - 1; -- Down
         when L =>
            Rope (Head).X := Rope (Head).X - 1; -- Left
         when R =>
            Rope (Head).X := Rope (Head).X + 1; -- Right
      end case; -- Direction
   end Step_Head;

   procedure Step_Knot (Rope : in out Ropes; Knot : in Knot_Indices) is

   begin --Step_Knot
      if abs (Rope (Rope_Indices'Pred (Knot)).X - Rope (Knot).X) > 1 or
      abs (Rope (Rope_Indices'Pred (Knot)).Y - Rope (Knot).Y) > 1 then
         -- Step required when knots are not overlaping or adjacent.
         if Rope (Rope_Indices'Pred (Knot)).X = Rope (Knot).X then
            -- Step Vertically
            if Rope (Rope_Indices'Pred (Knot)).Y < Rope (Knot).Y then
               Rope (Knot).Y := Rope (Knot).Y - 1;
            else
               Rope (Knot).Y := Rope (Knot).Y + 1;
            end if; -- Rope (Rope_Indices'Pred (Knot)).Y < Rope (Knot).Y
         elsif Rope (Rope_Indices'Pred (Knot)).Y = Rope (Knot).Y then
            -- Step Horizontally
            if Rope (Rope_Indices'Pred (Knot)).X < Rope (Knot).X then
               Rope (Knot).X := Rope (Knot).X - 1;
            else
               Rope (Knot).X := Rope (Knot).X + 1;
            end if; -- Rope (Rope_Indices'Pred (Knot)).X < Rope (Knot).X
         else
            -- Step Diagonally
            if Rope (Rope_Indices'Pred (Knot)).X < Rope (Knot).X then
               Rope (Knot).X := Rope (Knot).X - 1;
            else
               Rope (Knot).X := Rope (Knot).X + 1;
            end if; -- Rope (Rope_Indices'Pred (Knot)).X < Rope (Knot).X
            if Rope (Rope_Indices'Pred (Knot)).Y < Rope (Knot).Y then
               Rope (Knot).Y := Rope (Knot).Y - 1;
            else
               Rope (Knot).Y := Rope (Knot).Y + 1;
            end if; -- Rope (Rope_Indices'Pred (Knot)).Y < Rope (Knot).Y
         end if; -- Rope (Rope_Indices'Pred (Knot)).X = Rope (Knot).X
      end if; -- abs (Rope (Rope_Indices'Pred (Knot)).X - Rope (Knot).X) > 1 ...
   end Step_Knot;

   Move_Store : Move_Stores.Vector;
   Rope : Ropes:= (others => (0, 0));
   Knot_1_Set, Knot_9_Set : Coordinate_Sets.Set := Coordinate_Sets.Empty_Set;

begin -- December_09
   Read_Input (Move_Store);
   Include (Knot_1_Set, Rope (Knot_1));
   Include (Knot_9_Set, Rope (Knot_9));
   for M in Iterate (Move_Store) loop
      for Step in Positive range 1 .. Move_Store (M).Distance loop
         Step_Head (Rope, Move_Store (M).Direction);
         for K in Knot_Indices loop
            Step_Knot (Rope, K);
            Include (Knot_1_Set, Rope (Knot_1));
            Include (Knot_9_Set, Rope (Knot_9));
         end loop; -- K in Knot_Indices
      end loop; -- S in Positive range 1 .. Move_Store (M).Distance
   end loop; -- M in Iterate (Move_Store)
   Put_Line ("Part one:" & Length (Knot_1_Set)'Img);
   Put_Line ("Part two:" & Length (Knot_9_Set)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_09;
