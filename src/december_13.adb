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

   type Message_Elements;
   type Sub_Pointers is access Message_Elements;

   package Messages is new
     Ada.Containers.Doubly_Linked_Lists (Sub_Pointers);
   use Messages;

   type Message_Elements (Is_List : Boolean) is record
      case Is_List is
         when True =>
            Sub_Message : Messages.List := Messages.Empty_List;
         when False =>
            Value : Values;
      end case; -- Message_Elements
   end record; -- Message_Elements

   type Message_Pairs is record
      Left, Right : Sub_Pointers := null;
   end record; -- Message_Pairs

   package Message_Stores is new Ada.Containers.Vectors
     (Positive, Message_Pairs);
   use Message_Stores;

   type Comparison_Results is record
      Result  : Boolean := True;
      Decided : Boolean := False;
   end record; -- Comparison_Results

   Format_Error, Compare_Error : exception;

   procedure Read_Input (Message_Store : out Message_Stores.Vector) is

      procedure Read_Message (Text : in Unbounded_String;
                              Start_At : in out Positive;
                              Message : out Sub_Pointers) is

         First : Positive;
         Last : Natural;
         Node_Value : Values;
         Sub_Message : Sub_Pointers;
         Finished : Boolean := False;

      begin -- Read_Message
         if Start_At = 1 then
            if Element (Text, 1) = '[' then
               Message := new Message_Elements (True);
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
                  Sub_Message := new Message_Elements (True);
                  Read_Message (Text, Start_At, Sub_Message);
                  Append (Message.Sub_Message, Sub_Message);
               when ',' =>
                  Start_At := @ + 1;
               when ']' =>
                  Start_At := @ + 1;
                  Finished := True;
               when others =>
                  if '0' <= Element (Text, Start_At) and
                    Element (Text, Start_At) <= '9'
                  then
                     Find_Token
                       (Text, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
                     Node_Value := Values'Value (Slice (Text, First, Last));
                     Append (Message.Sub_Message, new Message_Elements (False));
                     Last_Element (Message.Sub_Message).Value := Node_Value;
                     Start_At := Last + 1;
                  else
                     raise Format_Error
                       with "Illegal symbol '" & Element (Text, Start_At) &
                       "'";
                  end if; --  '0' <= Element (Text, Start_At) and ...
            end case; -- Element (Text, Start_At)
         end loop; -- Start_At <= Length (Text)
      end Read_Message;

      Input_File : File_Type;
      Text : Unbounded_String;
      Message_Pair : Message_Pairs;
      Start_At : Positive;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_13.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Message_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Length (Text) > 0 then
            Start_At := 1;
            Read_Message (Text, Start_At, Message_Pair.Left);
            Get_Line (Input_File, Text);
            Start_At := 1;
            Read_Message (Text, Start_At, Message_Pair.Right);
            Append (Message_Store, Message_Pair);
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

   procedure Put (Message : in Sub_Pointers) is

   begin -- Put
      if Message.Is_List then
         Put ('[');
         for M in Iterate (Message.Sub_Message) loop
            if Element (M).Is_List then
               Put (Element (M));
            else
               Put (Element (M).Value'Img);
            end if; -- Element (M).Is_List
         end loop; -- M in Iterate (Message)
         Put (']');
      else
         Put (Message.Value'Img);
      end if; -- Message.Is_List
   end Put;

   function Compare (Message_Pair : in Message_Pairs) return Comparison_Results
   is

      Left : Messages.List renames Message_Pair.Left.Sub_Message;
      Right : Messages.List renames Message_Pair.Right.Sub_Message;
      Message_Pair_Out : Message_Pairs;
      Comparison_Result : Comparison_Results :=
        (Result => True, Decided => False);
      Result : Boolean renames Comparison_Result.Result;
      Decided : Boolean renames Comparison_Result.Decided;
      Returned : Comparison_Results;
      Lc, Rc : Messages.Cursor;

   begin -- Compare
      Lc := First (Left);
      Rc := First (Right);
      while Lc /= Messages.No_Element and Rc /= Messages.No_Element and
        not Decided loop
         if Element (Lc).Is_List then
            if Element (Rc).Is_List then
               -- Both Left and Right are Lists
               if Is_Empty (Element (Lc).Sub_Message) then
                  if Is_Empty (Element (Rc).Sub_Message) then
                     null; -- no decision, result unchanged
                  else
                     Decided := True;
                  end if; -- Is_Empty (Element (Rc).Sub_Message)
               else
                  if Is_Empty (Element (Rc).Sub_Message) then
                     Result := False;
                     Decided := True;
                  else
                     -- Neither Left nor Right are empty lists
                     Message_Pair_Out := (Left => Element (Lc),
                                          Right => Element (Rc));
                     Returned := Compare (Message_Pair_Out);
                     Result := @ and Returned.Result;
                     Decided := @ or Returned.Decided;
                  end if; -- Is_Empty (Element (Rc).Sub_Message)
               end if; -- Is_Empty (Element (Lc).Sub_Message)
            else
               -- Left is List and Right is Value
               declare
                  Right_List : Sub_Pointers := new Message_Elements (True);
                  Right_Value : Sub_Pointers := new Message_Elements (False);
               begin -- Right value becomes list
                  Right_Value.Value := Element (Rc).Value;
                  Append (Right_List.Sub_Message, Right_Value);
                  Message_Pair_Out := (Left => Element (Lc),
                                       Right => Right_List);
                  Returned := Compare (Message_Pair_Out);
                  Result := @ and Returned.Result;
                  Decided := @ or Returned.Decided;
               end; -- Right value becomes list
            end if; -- Element (Rc).Is_List
         else
            if Element (Rc).Is_List then
               -- Left is Value and Right is List
               declare
                  Left_List : Sub_Pointers := new Message_Elements (True);
                  Left_Value : Sub_Pointers := new Message_Elements (False);
               begin -- Left value becomes list
                  Left_Value.Value := Element (Lc).Value;
                  Append (Left_List.Sub_Message, Left_Value);
                  Message_Pair_Out := (Left => Left_List,
                                       Right => Element (Rc));
                  Returned := Compare (Message_Pair_Out);
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
      end loop; -- Lc /= Messages.No_Element  and Rc /= Messages.No_Element
      if not Decided then
         if Lc = Messages.No_Element then
            if Rc = Messages.No_Element then
               -- Both run out together, no decision
               null;
            else
               -- Left ran out first
               Decided := True;
            end if; -- Rc = Messages.No_Element
         else
            if Rc = Messages.No_Element then
               Result := False;
               Decided := True;
            end if; -- Rc = Messages.No_Element
            -- No else clause if both lists have remaining elements loop exited
            -- because the result was
         end if; -- Lc = Messages.No_Element
      end if; -- not Decided
      return Comparison_Result;
   end Compare;

   Message_Store : Message_Stores.Vector;
   Sum           : Natural := 0;

begin -- December_13
   Read_Input (Message_Store);
   for M in Iterate (Message_Store) loop
   if Compare (Message_Store (M)).Result then
      Sum := Sum + To_Index (M);
   end if; -- Compare (Message_Store (M). Left, Message_Store(M).Right) ...
   end loop; -- M in Iterate (Message_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
