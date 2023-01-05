with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_20 is

   subtype Positions is Natural;

   subtype Numbers is Long_Long_Integer;

   type Number_Nodes;

   type Node_Pointers is access Number_Nodes;

   type Number_Nodes is record
      Number : Numbers;
      Before, After : Node_Pointers := null;
   end record; -- Number_Nodes

   package Number_Maps is new
     Ada.Containers.Ordered_Maps (Positions, Node_Pointers);
   use Number_Maps;


   procedure Read_Input (Number_Map : in out Number_Maps.Map;
                        Zero_Ptr : out Node_Pointers) is

      Comma_Set : constant Character_Set := To_Set (',');
      Input_File      : File_Type;
      Text            : Unbounded_String;
      Start_At, First : Positive;
      Last            : Natural;
      Position : Positions := 0;
      Temp : Node_Pointers;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Number_Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Comma_Set, Start_At, Outside, First, Last);
         Temp := new Number_Nodes;
         Temp.Number := Numbers'Value (Slice (Text, First, Last));
         Include (Number_Map, Position, Temp);
         if Temp.Number = 0 then
            Zero_Ptr := Temp;
         end if; -- Temp.Number = 0
         Position := Position + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
      First_Element (Number_Map).Before := Last_Element (Number_Map);
      Last_Element (Number_Map).After := First_Element (Number_Map);
      Temp := Last_Element (Number_Map);
      for I in Iterate (Number_Map) loop
         Number_Map (I).Before := Temp;
         Temp.After := Number_Map (I);
         Temp := Number_Map (I);
      end loop; -- I in Iterate (Number_Map)
   exception
      when E : others =>
         Put_Line
           ("Line" & Positive_Count'Image (Line (Input_File) - 1) & " > " &
              Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Mixer (Number_Map : in Number_Maps.Map;
                    Zero_Ptr : in Node_Pointers) is

      -- The Mixing is a side effect on the ring structure.

      After, Before : Node_Pointers;
      Modulus : constant Numbers := Numbers (Length (Number_Map)) - 1;
      -- The - 1 I got from Redit. I think it is due to the unspecified
      -- behaviour when skipping more than the length of the list. Effectively a
      -- number does not skip itself. Alternately it could be seen as the number
      -- being unlinked from the ring before the skipping starts!

   begin -- Mixer
      for N in Iterate (Number_Map) loop
         if abs Number_Map (N).Number mod Modulus /= 0 then
            -- Unlink
            Before := Number_Map (N).Before;
            After :=  Number_Map (N).After;
            Before.After := After;
            After.Before := Before;
            -- Skip
            if Number_Map (N).Number > 0 then
               -- Forwads
               for I in Numbers range 1 ..
                 Number_Map (N).Number mod Modulus loop
                  Before := After;
                  After := After.After;
               end loop; --Number_Map (N).Number mod ...
            elsif  Number_Map (N).Number < 0 then
               -- Backwards
               for I in Numbers range 1 ..
               abs (Number_Map (N).Number) mod Modulus loop
                  After := Before;
                  Before := Before.Before;
               end loop; -- I in Numbers range 1 .. abs (Number_Map (N) ...
            end if; -- Number_Map (N).Number > 0
            -- Insert
            Before.After := Element (N);
            Number_Map (N).Before := Before;
            After.Before := Element (N);
            Number_Map (N).After := After;
         end if; -- abs Number_Map (N).Number) mod Numbers ...
      end loop; -- N in Iterate (Number_Map)
   end Mixer;

   function Sum (Number_Map : in Number_Maps.Map;
                 Zero_Ptr : in Node_Pointers) return Numbers is

      Result : Numbers := 0;
      Current : Node_Pointers := Zero_Ptr;

   begin -- Forward
      for S in Positive range 1 .. 3000 loop
         Current := Current.After;
         if S mod 1000 = 0 then
            Result := Result + Current.Number;
         end if; -- S mod 1000 = 0
      end loop; -- S in Positive range 1 .. Steps mod ...
      return Result;
   end Sum;

   Decription_Key : constant Numbers := 811589153;
   Number_Map : Number_Maps.Map;
   Zero_Ptr : Node_Pointers;

begin -- December_20
   Read_Input (Number_Map, Zero_Ptr);
   Mixer (Number_Map, Zero_Ptr);
   Put_Line ("Part one:" & Sum (Number_Map, Zero_Ptr)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Read_Input (Number_Map, Zero_Ptr);
   for I in Iterate (Number_Map) loop
      Number_Map (I).Number := Number_Map (I).Number * Decription_Key;
   end loop; -- I in Iterate (Number_Map)
   for T in Positive range 1 .. 10 loop
      Mixer (Number_Map, Zero_Ptr);
   end loop; -- T in Positive range 1 .. 10
   Put_Line ("Part two:" & Sum (Number_Map, Zero_Ptr)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_20;
