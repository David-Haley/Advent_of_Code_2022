with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_23 is

   Part_One_Rounds : constant Positive := 10;

   subtype Ordinates is Integer;

   type Coordinates is record
      X, Y : Ordinates;
   end record; -- Coordinates

   function "<" (Left, Right : Coordinates) return Boolean is
     (Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X));

   function "=" (Left, Right : Coordinates) return Boolean is
     (Left.X = Right.X and Left.Y = Right.Y);

   type Directions is (North, South, West, East);

   type Direction_Numbers is mod 4;

   type Elves is record
      Current, Proposed : Coordinates := (0, 0);
   end record; -- Elves

   package Elf_Lists is new Ada.Containers.Vectors (Positive, Elves);
   use Elf_Lists;

   package Use_Counts is new
     Ada.Containers.Ordered_Maps (Coordinates, Boolean);
   use Use_Counts;

   procedure Read_Input (Elf_List : out Elf_Lists.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Elf : Elves;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_23.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Elf_List);
      Elf.Current.Y := 1;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            if Element (Text, I) = '#' then
               Elf.Current.X := I;
               Elf.Proposed := Elf.Current;
               Append (Elf_List, Elf);
            end if; -- Element (Text, I) = '#'
         end loop; --  I in Positive range 1 .. Length (Text)
         Elf.Current.Y := Elf.Current.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Update (Elf_list : in out Elf_Lists.Vector;
                     Round : in Positive;
                     No_Movement : out Boolean) is

      procedure Count_Use (Elf_List : in Elf_Lists.Vector;
                           Is_Current : in Boolean;
                           Use_Count : out Use_Counts.Map) is

      begin -- Count_Use
         Clear (Use_Count);
         if Is_Current then
            for E in Iterate (Elf_List) loop
               if Contains (Use_Count, Elf_List (E).Current) then
                  raise Program_Error with "Already elf at " &
                    Elf_List (E).Current'Img;
               else
                  Include (Use_Count, Elf_List (E).Current, False);
               end if; -- Containes (Use_Count, Elf_List (I).Current)
            end loop; -- E in Iterate (Elf_List)
         else
            for E in Iterate (Elf_List) loop
               if Contains (Use_Count, Elf_List (E).Proposed) then
                  Use_Count (Elf_List (E).Proposed) := True;
               else
                  Include (Use_Count, Elf_List (E).Proposed, False);
               end if; -- Containes (Use_Count, Elf_List (I).Current)
            end loop; -- E in Iterate (Elf_List)
         end if; -- Is_Current
      end Count_Use;

      function Is_Clear (Use_Count : in Use_Counts.Map;
                         Elf : in Elves;
                         Direction : in Directions) return Boolean is

         Position : Coordinates;
         Result : Boolean;

      begin -- Is_Clear
         case Direction is
            when North =>
               Position.Y := Elf.Current.Y - 1;
               Position.X := Elf.Current.X - 1;
               Result := not Contains (Use_Count, Position);
               Position.X := Elf.Current.X;
               Result  :=  Result and not Contains (Use_Count, Position);
               Position.X := Elf.Current.X + 1;
               Result  :=  Result and not Contains (Use_Count, Position);
            when South =>
               Position.Y := Elf.Current.Y + 1;
               Position.X := Elf.Current.X - 1;
               Result := not Contains (Use_Count, Position);
               Position.X := Elf.Current.X;
               Result  :=  Result and not Contains (Use_Count, Position);
               Position.X := Elf.Current.X + 1;
               Result  :=  Result and not Contains (Use_Count, Position);
            when East =>
               Position.X := Elf.Current.X + 1;
               Position.Y := Elf.Current.Y - 1;
               Result := not Contains (Use_Count, Position);
               Position.Y := Elf.Current.Y;
               Result  :=  Result and not Contains (Use_Count, Position);
               Position.Y := Elf.Current.Y + 1;
               Result  :=  Result and not Contains (Use_Count, Position);
            when West =>
               Position.X := Elf.Current.X - 1;
               Position.Y := Elf.Current.Y - 1;
               Result := not Contains (Use_Count, Position);
               Position.Y := Elf.Current.Y;
               Result  :=  Result and not Contains (Use_Count, Position);
               Position.Y := Elf.Current.Y + 1;
               Result  :=  Result and not Contains (Use_Count, Position);
         end case; -- Direction
         Return Result;
      end Is_Clear;

      function Is_All_Clear (Use_Count : in Use_Counts.Map;
                             Elf : in Elves) return Boolean is
        (Is_Clear (Use_Count, Elf, North) and
             Is_Clear (Use_Count, Elf, South) and
             Is_Clear (Use_Count, Elf, East) and
             Is_Clear (Use_Count, Elf, West));

      procedure Propose_Move (Elf : in out Elves;
                              Direction : in Directions) is

      begin -- Propose_Move
         case Direction is
            when North =>
               Elf.Proposed.Y := Elf.Current.Y - 1;
            when South =>
               Elf.Proposed.Y := Elf.Current.Y + 1;
            when East =>
               Elf.Proposed.X := Elf.Current.X + 1;
            when West =>
               Elf.Proposed.X := Elf.Current.X - 1;
         end case; -- Direction
      end Propose_Move;

      pragma Inline_Always (Is_Clear, Is_All_Clear, Propose_Move);

      Current_Use, Proposed_Use : Use_Counts.Map;
      N_Dir : Direction_Numbers;
      Direction : Directions;

   begin -- Update
      -- Consider Moves
      No_Movement := True;
      Count_Use (Elf_List, True, Current_Use);
      for E in Iterate (Elf_List) loop
         if not Is_All_Clear (Current_Use, Elf_List (E)) then
            N_Dir := 0;
            loop -- test each direction in rotating order
               Direction :=
                 Directions'Val (Direction_Numbers'Mod (Round - 1) + N_Dir);
               if Is_Clear (Current_Use, Elf_List (E), Direction) then
                  Propose_Move (Elf_List (E), Direction);
                  No_Movement := False;
               end if; -- Is_Clear (Current_Use, Elf_List (E), Direction)
               exit when N_Dir = Direction_Numbers'Last or
                 Elf_List (E).Current /= Elf_List (E).Proposed;
               N_Dir := N_Dir + 1;
            end loop; -- test each direction in rotating order
         end if; -- not Is_All_Clear (Current_Use, Elf_List (E))
      end loop; -- Iterate (Elf_List)
      -- Execute moves
      Count_Use (Elf_List, False, Proposed_Use);
      for E in Iterate (Elf_List) loop
         if Proposed_Use (Elf_List (E).Proposed) then
            -- More than one Elf proposes to move here, revoke move
            Elf_List (E).Proposed := Elf_List (E).Current;
         else
            -- Only one proposal to move here, move
            Elf_List (E).Current := Elf_List (E).Proposed;
         end if; -- Proposed_Use (Elf_List (E).Proposed)
      end loop; -- E in Iterate (Elf_List)
   end Update;

   function Vacant_Tiles (Elf_List : in out Elf_Lists.Vector) return Natural is

      procedure Limits (Elf_list : in Elf_Lists.Vector;
                        Xl, Yl, Xh, Yh : out Ordinates) is
      begin -- Limits
         Xl := Ordinates'Last;
         Yl := Ordinates'Last;
         Xh := Ordinates'First;
         Yh := Ordinates'First;
         for E in Iterate (Elf_List) loop
            if Xl > Elf_List (E).Current.X then
               Xl := Elf_List (E).Current.X;
            end if; -- Xl > Elf_List (E).Current.X
            if Yl > Elf_List (E).Current.Y then
               Yl := Elf_List (E).Current.Y;
            end if; -- Yl > Elf_List (E).Current.Y
            if Xh < Elf_List (E).Current.X then
               Xh := Elf_List (E).Current.X;
            end if; -- Xh < Elf_List (E).Current.X
            if Yh < Elf_List (E).Current.Y then
               Yh := Elf_List (E).Current.Y;
            end if; -- Yh < Elf_List (E).Current.Y
         end loop; -- E in Iterate (Elf_List)
      end Limits;

      No_Movement : Boolean;
      Xl, Yl, Xh, Yh : Ordinates;

   begin -- Vacant_Tiles
      for Round in Positive range 1 .. Part_One_Rounds loop
         Update (Elf_List, Round, No_Movement);
      end loop; -- Round in Positive range 1 .. Part_One_Rounds
      Limits (Elf_list, Xl, Yl, Xh, Yh);
      return (Xh - Xl + 1) * (Yh - Yl + 1) - Natural (Length (Elf_List));
   end Vacant_Tiles;

   Function Final_Movement (Elf_List : in out Elf_Lists.Vector)
                            return Positive is

      No_Movement : Boolean;
      Round : Positive := Part_One_Rounds + 1;

   begin -- Final_Movement
      loop -- Until no movement
         Update (Elf_list, Round, No_Movement);
         exit when No_Movement;
         Round := Round + 1;
      end loop; -- Until no movement
      return Round;
   end Final_Movement;

   Elf_List : Elf_Lists.Vector;

begin -- December_32
   Read_Input (Elf_List);
   Put_Line ("Part one:" & Vacant_Tiles (Elf_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Final_Movement (Elf_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_23;
