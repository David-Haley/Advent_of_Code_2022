with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_15 is

   subtype Ordinates is Integer;

   package Ordinate_Sets is new Ada.Containers.Ordered_Sets (Ordinates);

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is

   begin -- "<"
      return Left.X < Right.X or (Left.X = Right.X and Left.Y < Right.Y);
   end "<";

   function "=" (Left, Right : Coordinates) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Beacon_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Beacon_Sets;

   type Sensors is record
      Nearest_Beacon : Coordinates;
      Distance : Natural;
   end record;

   type Segments is record
      X1, X2 : Ordinates;
   end record; -- Segments

   package Segment_Lists is new
     Ada.Containers.Ordered_Maps (Ordinates, Ordinates);
   use Segment_Lists;

   package Sensor_Lists is new
     Ada.Containers.Ordered_Maps (Coordinates, Sensors);
   use Sensor_Lists;

   function Man_Dist (A, B : in Coordinates) return Natural is
     (abs (A.X - B.X) + abs (A.Y - B.Y));

   procedure Read_Input (Sensor_List : out Sensor_Lists.Map;
                         Beacon_Set : out Beacon_Sets.Set;
                         Test_Row : out Ordinates;
                         Bound : out Natural) is

      Integer_Set : Character_Set := To_Set ("-0123456789");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Sensor : Sensors;
      Sensor_Position : Coordinates;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_15.txt");
         Test_Row := 2000000;
         Bound := 4000000;
      else
         Open (Input_File, In_File, Argument (1));
         Test_Row := Ordinates'Value (Argument (2));
         Bound := Natural'Value (Argument (3));
      end if; -- Argument_Count = 0
      Clear (Sensor_List);
      Clear (Beacon_Set);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Integer_Set, Start_at, Inside, First, Last);
         Sensor_Position.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_at, Inside, First, Last);
         Sensor_Position.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_at, Inside, First, Last);
         Sensor.Nearest_Beacon.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_at, Inside, First, Last);
         Sensor.Nearest_Beacon.Y := Ordinates'Value (Slice (Text, First, Last));
         Sensor.Distance := Man_Dist (Sensor_Position, Sensor.Nearest_Beacon);
         Include (Sensor_List, Sensor_Position, Sensor);
         Include (Beacon_Set, Sensor.Nearest_Beacon);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Position_Count (Sensor_List : in Sensor_Lists.Map;
                            Beacon_Set : in Beacon_Sets.Set;
                            Test_Row : in Ordinates) return Natural is

      use Ordinate_Sets;

      Position_Set : Ordinate_Sets.Set := Ordinate_Sets.Empty_Set;
      X1, X2 : Ordinates;

   begin -- Position_Count
      for S in Iterate (Sensor_List) loop
         if abs (Key (S).Y - Test_Row) <= Sensor_List (S).Distance then
            -- some elements positions can't be beacons
            X1 := Key (S).X - (Sensor_List (S).Distance -
                               abs (Key (S).Y - Test_Row));
            X2 := Key (S).X + (Sensor_List (S).Distance -
                               abs (Key (S).Y - Test_Row));
            for X in Ordinates range X1 .. X2 loop
               Include (Position_Set, X);
            end loop; -- X in Ordinates range X1 .. X2
         end if; -- abs (Key (S).Y - Test_Row) <= Sensor_List (S).Distance
      end loop; -- S in Iterate (Sensor_List)
      for B in Iterate (Beacon_Set) loop
         if Element (B).Y = Test_Row then
            Exclude (Position_Set, Element (B).X);
         end if; -- Element (B).Y = Test_Row)
      end loop;-- B in Iterate (Beacon_Set)
      return Natural (Length (Position_Set));
   end Position_Count;

   Function Search (Sensor_List : in Sensor_Lists.Map;
                    Beacon_Set : in Beacon_Sets.Set;
                    Bound : in Ordinates) return Unsigned_64 is

      procedure Update (Segment_List : in out Segment_Lists.Map;
                        Segment : in Segments) is

         -- Manages a non overlapping list of line segments representing regions
         -- which are undetectable.

         Sc : Segment_Lists.Cursor := First (Segment_List);
         Temp : Segments;

      begin -- Update
         while Sc /= Segment_Lists.No_Element loop
            if Segment.X1 <= Key (Sc) and
              Segment_List (Sc) <= Segment.X2 then
               -- Segment in list fully contained in segment to be removed
               Delete (Segment_List, Sc);
               Sc := Segment_Lists.No_Element;
            elsif Key (Sc) <= Segment.X1 and
              Segment.X2 <= Segment_List (Sc) then
               -- Segment in list fully contains segment to be removed splits
               -- list segment into two.
               Temp.X1 := Segment.X2 + 1;
               Temp.X2 := Segment_List (Sc);
               if Segment.X1 - 1 >= Key (Sc) then
                  Segment_List (Sc) := Segment.X1 - 1;
               else
                  Delete (Segment_List, Sc);
                  Sc := Segment_Lists.No_Element;
               end if; -- Segment.X1 - 1 >= Key (Sc)
               if Temp.X1 <= Temp.X2 then
                  Include (Segment_List, Temp.X1, Temp.X2);
               end if; -- Temp.X1 <= Temp.X2
            elsif Segment.X1 <= Key (Sc) and
              Key (Sc) <= Segment.X2 then
               -- Lower end of list is contained in segment to be removed
               Temp.X1 := Segment.X2 + 1;
               Temp.X2 := Segment_List (Sc);
               Delete (Segment_List, Sc);
               Sc := Segment_Lists.No_Element;
               if Temp.X1 <= Temp.X2 then
                  Include (Segment_List, Temp.X1, Temp.X2);
               end if; -- Temp.X1 <= Temp.X2
            elsif Segment.X1 <= Segment_List (Sc) and
              Segment_List (Sc) <= Segment.X2 then
               -- Upper end of list is contained in segment to be removed
               if Segment.X1 - 1 >= Key (Sc) then
                  Segment_List (Sc) := Segment.X1 - 1;
               else
                  Delete (Segment_List, Sc);
                  Sc := Segment_Lists.No_Element;
               end if; -- Segment.X1 - 1 >= Key (Sc)
            end if; -- Segment.X1 <= Key (Sc) and ...
            if Sc = Segment_Lists.No_Element then
               Sc := First (Segment_List);
            else
               Next (Sc);
            end if; -- Sc = Segment_Lists.No_Element
         end loop; -- Sc /= Segment_Lists.No_Element
      end Update;

      Segment_List : Segment_Lists.Map;
      Y : Ordinates := 0;
      Segment : Segments;

   begin -- Search
      loop -- Iterate on Y
         Clear (Segment_List);
         Include (Segment_List, 0, Bound);
         for S in Iterate (Sensor_List) loop
            Segment.X1 := Key (S).X - (Sensor_List (S).Distance -
                                       abs (Key (S).Y - Y));
            Segment.X2 := Key (S).X + (Sensor_List (S).Distance -
                                       abs (Key (S).Y - Y));
            -- Reduce segment to area of interest
            if Segment.X1 < 0 then
               Segment.X1 := 0;
            end if; -- Segment.X1 < 0
            if Segment.X2 > Bound then
               Segment.X2 := Bound;
            end if; -- Segment.X2 > Bound
            if Segment.X1 <= Segment.X2 then
               -- check that segment is valid
               Update (Segment_List, Segment);
            end if; -- Segment.X1 < Segment.X2
         end loop; -- S in Iterate (Sensor_List)
         exit when Y >= Bound or
           (Length (Segment_List) = 1 and then
                (First_Key (Segment_List) =
                       First_Element (Segment_List)));
         Y := Y + 1;
      end loop; -- Iterate on Y
      return Unsigned_64 (Y) +
        Unsigned_64(First_Element (Segment_List)) * 4000000;
   end Search;

   Sensor_List : Sensor_Lists.Map;
   Beacon_Set : Beacon_Sets.Set;
   Test_Row : Ordinates;
   Bound : Natural;

begin -- December_15
   Read_Input (Sensor_List, Beacon_Set, Test_Row, Bound);
   Put_Line ("Part one:" &
               Position_Count (Sensor_List, Beacon_Set, Test_Row)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" &
               Search (Sensor_List, Beacon_Set, Bound)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_15;
