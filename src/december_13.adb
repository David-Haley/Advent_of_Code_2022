with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with DJH.Execution_Time;         use DJH.Execution_Time;

procedure December_13 is

   subtype Values is Natural;

   type Packet_Elements;
   type Packet_Pointers is access Packet_Elements;

   package Packets is new
     Ada.Containers.Doubly_Linked_Lists (Packet_Pointers);
   use Packets;

   type Packet_Elements (Is_List : Boolean) is record
      case Is_List is
         when True =>
            Sub_Packet : Packets.List := Packets.Empty_List;
         when False =>
            Value : Values;
      end case; -- Packet_Elements
   end record; -- Packet_Elements

   type Packet_Pairs is record
      Left, Right : Packet_Pointers := null;
   end record; -- Packet_Pairs

   package Packet_Pair_Stores is new Ada.Containers.Vectors
     (Positive, Packet_Pairs);
   use Packet_Pair_Stores;

   package Packet_Stores is new Ada.Containers.Vectors
     (Positive, Packet_Pointers);
   use Packet_Stores;

   type Comparison_Results is record
      Result  : Boolean := True;
      Decided : Boolean := False;
   end record; -- Comparison_Results

   Format_Error, Compare_Error : exception;

   procedure Read_Input (Packet_Pair_Store : out Packet_Pair_Stores.Vector) is

      procedure Read_Packet (Text : in Unbounded_String;
                             Start_At : in out Positive;
                             Packet : out Packet_Pointers) is

         First : Positive;
         Last : Natural;
         Node_Value : Values;
         Sub_Packet : Packet_Pointers;
         Finished : Boolean := False;

      begin -- Read_Packet
         if Start_At = 1 then
            if Element (Text, 1) = '[' then
               Packet := new Packet_Elements (True);
               Start_At := @ + 1;
            else
               raise Format_Error
                 with "Expected '[' and found '" & Element (Text, 1) & "'";
            end if; -- Element (Text, 1) /= '['
         end if; -- Start_At = 1
         while Start_At <= Length (Text)  and not Finished loop
            case Element (Text, Start_At) is
               when '[' =>
                  Start_At := @ + 1;
                  Sub_Packet := new Packet_Elements (True);
                  Read_Packet (Text, Start_At, Sub_Packet);
                  Append (Packet.Sub_Packet, Sub_Packet);
               when ',' =>
                  Start_At := @ + 1;
               when ']' =>
                  Start_At := @ + 1;
                  Finished := True;
               when others =>
                  if Is_In (Element (Text, Start_At), Decimal_Digit_Set) then
                     Find_Token (Text, Decimal_Digit_Set, Start_At, Inside,
                                 First, Last);
                     Node_Value := Values'Value (Slice (Text, First, Last));
                     Append (Packet.Sub_Packet, new Packet_Elements (False));
                     Last_Element (Packet.Sub_Packet).Value := Node_Value;
                     Start_At := Last + 1;
                  else
                     raise Format_Error
                       with "Illegal symbol '" & Element (Text, Start_At) &
                       "'";
                  end if; -- Is_In (Element (Text, Start_At), Decimal_Digit_Set)
            end case; -- Element (Text, Start_At)
         end loop; -- Start_At <= Length (Text)
      end Read_Packet;

      Input_File : File_Type;
      Text : Unbounded_String;
      Packet_Pair : Packet_Pairs;
      Start_At : Positive;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_13.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Packet_Pair_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Length (Text) > 0 then
            Start_At := 1;
            Read_Packet (Text, Start_At, Packet_Pair.Left);
            Get_Line (Input_File, Text);
            Start_At := 1;
            Read_Packet (Text, Start_At, Packet_Pair.Right);
            Append (Packet_Pair_Store, Packet_Pair);
         end if; -- Length (Text) > 0
      end loop; -- not End_Of_File (Input_File)
   exception
      when E : others =>
         Put_Line
           ("Line" & Positive_Count'Image (Line (Input_File) - 1) & " > " &
              Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Compare (Packet_Pair : in Packet_Pairs) return Comparison_Results is

      Left : Packets.List renames Packet_Pair.Left.Sub_Packet;
      Right : Packets.List renames Packet_Pair.Right.Sub_Packet;
      Packet_Pair_Out : Packet_Pairs;
      Comparison_Result : Comparison_Results; -- Initialised in declaration
      Result : Boolean renames Comparison_Result.Result;
      Decided : Boolean renames Comparison_Result.Decided;
      Returned : Comparison_Results;
      Lc, Rc : Packets.Cursor;

   begin -- Compare
      Lc := First (Left);
      Rc := First (Right);
      while Lc /= Packets.No_Element and Rc /= Packets.No_Element and
        not Decided loop
         if Element (Lc).Is_List then
            if Element (Rc).Is_List then
               -- Both Left and Right are Lists
               if Is_Empty (Element (Lc).Sub_Packet) then
                  if Is_Empty (Element (Rc).Sub_Packet) then
                     null; -- no decision, result unchanged
                  else
                     Decided := True;
                  end if; -- Is_Empty (Element (Rc).Sub_Packet)
               else
                  if Is_Empty (Element (Rc).Sub_Packet) then
                     Result := False;
                     Decided := True;
                  else
                     -- Neither Left nor Right are empty lists
                     Packet_Pair_Out := (Left => Element (Lc),
                                         Right => Element (Rc));
                     Returned := Compare (Packet_Pair_Out);
                     Result := @ and Returned.Result;
                     Decided := @ or Returned.Decided;
                  end if; -- Is_Empty (Element (Rc).Sub_Packet)
               end if; -- Is_Empty (Element (Lc).Sub_Packet)
            else
               -- Left is List and Right is Value
               declare
                  Right_List : Packet_Pointers := new Packet_Elements (True);
                  Right_Value : Packet_Pointers := new Packet_Elements (False);
               begin -- Right value becomes list
                  Right_Value.Value := Element (Rc).Value;
                  Append (Right_List.Sub_Packet, Right_Value);
                  Packet_Pair_Out := (Left => Element (Lc),
                                      Right => Right_List);
                  Returned := Compare (Packet_Pair_Out);
                  Result := @ and Returned.Result;
                  Decided := @ or Returned.Decided;
               end; -- Right value becomes list
            end if; -- Element (Rc).Is_List
         else
            if Element (Rc).Is_List then
               -- Left is Value and Right is List
               declare
                  Left_List : Packet_Pointers := new Packet_Elements (True);
                  Left_Value : Packet_Pointers := new Packet_Elements (False);
               begin -- Left value becomes list
                  Left_Value.Value := Element (Lc).Value;
                  Append (Left_List.Sub_Packet, Left_Value);
                  Packet_Pair_Out := (Left => Left_List, Right => Element (Rc));
                  Returned := Compare (Packet_Pair_Out);
                  Result := @ and Returned.Result;
                  Decided := @ or Returned.Decided;
               end; -- Left value becomes list
            else
               -- Both Left and Right are Values
               Result := @ and Element (Lc).Value <= Element (Rc).Value;
               Decided := @ or not Result or
                 Element (Lc).Value < Element (Rc).Value;
            end if; -- Element (Rc).Is_List
         end if; -- Element (Lc).Is_List
         if not Decided then
            Next (Lc);
            Next (Rc);
         end if; -- not Decided
      end loop; -- Lc /= Packets.No_Element  and Rc /= Packets.No_Element
      if not Decided then
         if Lc = Packets.No_Element then
            if Rc = Packets.No_Element then
               -- Both run out together, no decision
               null;
            else
               -- Left ran out first
               Decided := True;
            end if; -- Rc = Packets.No_Element
         else
            if Rc = Packets.No_Element then
               -- Right ran out first
               Result := False;
               Decided := True;
            end if; -- Rc = Packets.No_Element
            -- No else clause if both lists have remaining elements loop exited
            -- because the result was already decided.
         end if; -- Lc = Packets.No_Element
      end if; -- not Decided
      return Comparison_Result;
   end Compare;

   function "<" (Left, Right : Packet_Pointers) return Boolean is

      Packet_Pair : Packet_Pairs := (Left, Right);
      Comparison_Result : Comparison_Results := Compare (Packet_Pair);

   begin -- "<"
      return Comparison_Result.Result and Comparison_Result.Decided;
   end "<";

   function "=" (Left, Right : Packet_Pointers) return Boolean is

      Packet_Pair : Packet_Pairs := (Left, Right);
      Reverse_Pair : Packet_Pairs := (Right, Left);

   begin -- "="
      -- A <= B and B <= A implies A = B
      return Compare (Packet_Pair).Result and Compare (Reverse_Pair).Result;
   end "=";

   package Packet_Sorting is new Packet_Stores.Generic_Sorting;

   Packet_Pair_Store : Packet_Pair_Stores.Vector;
   Packet_Store : Packet_Stores.Vector := Packet_Stores.Empty_Vector;
   Packet_2, Packet_6, Temp : Packet_Pointers;
   Sum : Natural := 0;
   Product : Positive := 1;

begin -- December_13
   Read_Input (Packet_Pair_Store);
   for P in Iterate (Packet_Pair_Store) loop
      if Compare (Packet_Pair_Store (P)).Result then
         Sum := @ + To_Index (P);
      end if; --Compare (Packet_Pair_Store (P)).Result
      -- required for part two
      Append (Packet_Store, Packet_Pair_Store (P).Left);
      Append (Packet_Store, Packet_Pair_Store (P).Right);
   end loop; -- P in Iterate (Packet_Pair_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   -- Build [[2]] and append to Packet_Store
   Packet_2 := new Packet_Elements (True);
   Append (Packet_2.Sub_Packet, new Packet_Elements (True));
   Temp := new Packet_Elements (False);
   Temp.Value := 2;
   Append (First_element (Packet_2.Sub_Packet).Sub_Packet, Temp);
   Append (Packet_Store, Packet_2);
   -- Build [[6]] and append to Packet_Store
   Packet_6 := new Packet_Elements (True);
   Append (Packet_6.Sub_Packet, new Packet_Elements (True));
   Temp := new Packet_Elements (False);
   Temp.Value := 6;
   Append (First_element (Packet_6.Sub_Packet).Sub_Packet, Temp);
   Append (Packet_Store, Packet_6);
   Packet_Sorting.Sort (Packet_Store);
   for P in iterate (Packet_Store) loop
      if Packet_Store (P) = Packet_2 or Packet_Store (P) = Packet_6 then
         Product := @ * To_Index (P);
      end if; --  Packet_Store (P) = Packet_2 or Packet_Store (P) = Packet_6
   end loop; -- P in iterate (Packet_Store)
   Put_Line ("Part two:" & Product'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
