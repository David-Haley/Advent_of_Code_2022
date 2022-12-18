with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_18 is

   subtype Ordinates is Integer range -2 .. Integer'Last;
   -- All coordinates are Natural; however need to allow for Contains call -1.
   -- Also to allow for an outside -2.

   type Coordinates is record
      X : Ordinates;
      Y : Ordinates;
      Z : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.X < Right.X or
        (Left.X = Right.X and (Left.Y < Right.Y or
             (Left.Y = Right.Y and Left.Z < Right.Z))));

   function "=" (Left, Right : Coordinates) return Boolean is
     (Left.X = Right.X and Left.Y = Right.Y and Left.Z = Right.Z);

   package Cube_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
   use Cube_Sets;

   subtype Face_Counts is Natural range 0 .. 6;

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Coordinates);

   package Fill_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);

   procedure Read_Input (Cube_Set : out Cube_Sets.Set) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Coordinate : Coordinates;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_18.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Cube_Set);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.X := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.Y := Ordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Coordinate.Z := Ordinates'Value (Slice (Text, First, Last));
         Include (Cube_Set, Coordinate);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Adjacent_Count (Cube_Set : in Cube_Sets.Set;
                            Coordinate : in Coordinates) return Face_Counts is

      Face_Count : Natural := 0;

   begin -- Adjacent_Count
      if Contains (Cube_Set, (Coordinate.X - 1, Coordinate.Y,
                   Coordinate.Z)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X - 1, Coordinate.Y, ...
      if Contains (Cube_Set, (Coordinate.X + 1, Coordinate.Y,
                   Coordinate.Z)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X + 1, Coordinate.Y, ...
      if Contains (Cube_Set, (Coordinate.X, Coordinate.Y - 1,
                   Coordinate.Z)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X, Coordinate.Y - 1, ...
      if Contains (Cube_Set, (Coordinate.X, Coordinate.Y + 1,
                   Coordinate.Z)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X, Coordinate.Y + 1, ...
      if Contains (Cube_Set, (Coordinate.X, Coordinate.Y,
                   Coordinate.Z - 1)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X, Coordinate.Y, ...
      if Contains (Cube_Set, (Coordinate.X, Coordinate.Y,
                   Coordinate.Z + 1)) then
         Face_Count := Face_Count + 1;
      end if; -- Contains (Cube_Set, (Coordinate.X, Coordinate.Y, '''
      return Face_Count;
   end Adjacent_Count;

   procedure Limits (Cube_Set : in Cube_Sets.Set;
                     Xl, Yl, Zl : out Ordinates;
                     Xh, Yh, Zh : out Ordinates) is

   begin -- Limits
      Xl := Ordinates'Last;
      Yl := Ordinates'Last;
      Zl := Ordinates'Last;
      Xh := Ordinates'First;
      Yh := Ordinates'First;
      Zh := Ordinates'First;
      for C in Iterate (Cube_Set) loop
         if Element (C).X < Xl then
            Xl := Element (C).X;
         end if;
         if Element (C).Y < Yl then
            Yl := Element (C).Y;
         end if;
         if Element (C).Z < Zl then
            Zl := Element (C).Z;
         end if;
         if Element (C).X > Xh then
            Xh := Element (C).X;
         end if;
         if Element (C).Y > Yh then
            Yh := Element (C).Y;
         end if;
         if Element (C).Z > Zh then
            Zh := Element (C).Z;
         end if;
      end loop;
   end Limits;

   procedure Fill_Outside (Cube_Set : in Cube_Sets.Set;
                           Water_Set : out Cube_Sets.Set) is

      function Is_Water (Cube_Set : in Cube_Sets.Set;
                         Water_Set : in Cube_Sets.Set;
                         Xl, Yl, Zl : in Ordinates;
                         Xh, Yh, Zh : in Ordinates;
                         Next_Test : in Coordinates) return Boolean is

         -- More correctly new water, that is, not Cube_Set or water already in
         -- Water_Set.

      begin -- Is_Water
         return not Contains (Cube_Set, Next_Test) and
           not Contains (Water_Set, Next_Test) and
           Xl <= Next_Test.X and Next_Test.X <= Xh and
           Yl <= Next_Test.Y and Next_Test.Y <= Yh and
           Zl <= Next_Test.Z and Next_Test.Z <= Zh;
      end Is_Water;

      Xl, Yl, Zl : Ordinates;
      Xh, Yh, Zh : Ordinates;
      Current, Next_Test : Coordinates;
      Fill_Queue : Fill_Queues.Queue;

   begin -- Fill_Outside

      Limits (Cube_Set, Xl, Yl, Zl, Xh, Yh, Zh);
      -- Allow for at least one cube of water within limits;
      Xl := Xl - 1;
      Xh := Xh + 1;
      Yl := Yl - 1;
      Yh := Yh + 1;
      Zl := Zl - 1;
      Zh := Zh + 1;
      Current := (Xl, Yl, Zl); -- Guaranteed to not be part of Cube
      Fill_Queue.Enqueue (Current);
      Include (Water_Set, (Xl, Yl, Zl));
      while Fill_Queue.Current_Use > 0 loop
         Fill_Queue.Dequeue (Current);
         Next_Test.X := Current.X - 1;
         Next_Test.Y := Current.Y;
         Next_Test.Z := Current.Z;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
         Next_Test.X := Current.X + 1;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
         Next_Test.X := Current.X;
         Next_Test.Y := Current.Y - 1;
         Next_Test.Z := Current.Z;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
         Next_Test.Y := Current.Y + 1;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
         Next_Test.X := Current.X;
         Next_Test.Y := Current.Y;
         Next_Test.Z := Current.Z - 1;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
         Next_Test.Z := Current.Z + 1;
         if Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh,
                      Next_Test) then
            Include (Water_Set, Next_Test);
            Fill_Queue.Enqueue (Next_Test);
         end if; -- Is_Water (Cube_Set, Water_Set, Xl, Yl, Zl, Xh, Yh, Zh, ...
      end loop; -- Fill_Queue.Current_Use > 0
   end Fill_Outside;

   procedure Put (Cube_Set : in Cube_Sets.Set;
                  Water_Set : in Cube_Sets.Set) is

      Xl, Yl, Zl : Ordinates;
      Xh, Yh, Zh : Ordinates;

   begin -- Put
      Limits (Cube_Set, Xl, Yl, Zl, Xh, Yh, Zh);
      -- Allow for at least one cube of water within limits;
      Xl := Xl - 1;
      Xh := Xh + 1;
      Yl := Yl - 1;
      Yh := Yh + 1;
      Zl := Zl - 1;
      Zh := Zh + 1;
      Put_Line ("Limits (" & Xl'Img & " .." & Xh'Img & ','
                & Yl'Img & " .." & Yh'Img & ','
                & Xl'Img & " .." & Xh'Img & ')');
      for Z in reverse Ordinates range Zl .. Zh loop
         Put_Line ("Slice Z:" & Z'Img);
         for Y in reverse Ordinates range Yl .. Yh loop
            for X in Ordinates range Xl .. Xh loop
               if Contains (Cube_Set, (X, Y, Z)) then
                  Put ('#');
               elsif Contains (Water_Set, (X, Y, Z)) then
                  Put ('.');
               else
                  Put ('?');
               end if; -- Contains (Cube_Set, (X, Y, Z))
            end loop; -- X in Ordinates range Xl .. Xh
            New_Line;
         end loop; -- Y in reverse Ordinates range Yl .. Yh
         New_Line;
      end loop;-- Z in reverse Ordinates range Zl .. Zh
   end Put;

   Cube_Set, Water_Set: Cube_Sets.Set;
   Exposed_Face_Count, Outside_Face_Count : Natural := 0;

begin -- December_18
   Read_Input (Cube_Set);
   for C in Iterate (Cube_Set) loop
      Exposed_Face_Count := Exposed_Face_Count +
        (Face_Counts'Last - Adjacent_Count (Cube_Set, Element (C)));
   end loop; -- C in Iterate (Cube_Set)
   Fill_Outside (Cube_Set, Water_Set);
   for C in Iterate (Cube_Set) loop
      Outside_Face_Count := Outside_Face_Count +
        Adjacent_Count (Water_Set, Element (C));
   end loop; -- C in Iterate (Cube_Set)
   Put (Cube_Set, Water_Set);
   Put_Line ("Part one:" & Exposed_Face_Count'Img);
   Put_Line ("Part two:" & Outside_Face_Count'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_18;
