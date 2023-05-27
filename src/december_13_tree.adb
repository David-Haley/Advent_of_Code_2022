with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Vectors;
with DJH.Execution_Time;         use DJH.Execution_Time;

procedure December_13 is

   subtype Node_Values is Integer range -1 .. Integer'Last;
   subtype Read_Values is Node_Values range 0 .. Integer'Last;
   Empty : constant Node_Values := Node_Values'First;

   package Messages is new Ada.Containers.Multiway_Trees (Node_Values);
   use Messages;

   type Message_Pairs is record
      Right, Left : Messages.Tree;
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

      procedure Read_Message
        (Text : in Unbounded_String; Message : out Messages.Tree)
      is

         Start_At, First : Positive;
         Last            : Natural;
         Tc              : Messages.Cursor := Messages.No_Element;
      -- The cursor is intended to be the parent for any items to be appended
      -- to. Nodes used puerly for linkage have values -1. Nodes with values
      -- other than -1 should have no children, that is, be leaf nodes.
         Node_Value : Node_Values;

      begin -- Read_Message
         if Element (Text, 1) = '[' then
            Tc := Root (Message);
         else
            raise Format_Error
              with "Expected '[' and found '" & Element (Text, 1) & "'";
         end if; -- Element (Text, 1) = '['
         Clear (Message);
         Start_At := 1;
         while Start_At <= Length (Text) loop
            case Element (Text, Start_At) is
               when '[' =>
                  Append_Child (Message, Tc, Empty);
                  Tc       := Last_Child (Tc);
                  Start_At := Start_At + 1;
               when ',' =>
                  Start_At := Start_At + 1;
               when ']' =>
                  Tc       := Parent (Tc);
                  Start_At := Start_At + 1;
               when others =>
                  if '0' <= Element (Text, Start_At) and
                    Element (Text, Start_At) <= '9'
                  then
                     Find_Token
                       (Text, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
                     Node_Value :=
                       Read_Values'Value (Slice (Text, First, Last));
                     Append_Child (Message, Tc, Node_Value);
                     Start_At := Last + 1;
                  else
                     raise Format_Error
                       with "Illegal symbol '" & Element (Text, Start_At) &
                       "'";
                  end if; --  '0' <= Element (Text, Start_At) and ...
            end case; -- Element (Text, Start_At)
         end loop; -- Start_At <= Length (Text)
         if Root (Message) /= Tc then
            raise Format_Error with "Parse failure did not return to root";
         end if; -- Root (Tree) /= Tc
      end Read_Message;

      Input_File   : File_Type;
      Text         : Unbounded_String;
      Message_Pair : Message_Pairs;

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
            Read_Message (Text, Message_Pair.Left);
            Get_Line (Input_File, Text);
            Read_Message (Text, Message_Pair.Right);
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

   procedure Put (Message : in Messages.Tree) is

   begin -- Put
      Put_Line ("Nodes:" & Node_Count (Message)'Img);
      for M in Iterate (Message) loop
         for I in Positive range 1 .. Positive (Depth (M)) loop
            Put ("   ");
         end loop; -- I in Positive range 1 .. Positive (Depth (M))
         Put_Line (Element (M)'Img);
      end loop; -- M in Iterate (Message)
      Put_Line ("end Put");
   end Put;

   function Compare (Left, Right : in Messages.Tree) return Comparison_Results
   is

      L_Cc : Messages.Cursor := First_Child (Root (Left));
      R_Cc : Messages.Cursor := First_Child (Root (Right));
      S_Tc                          : Messages.Cursor;
      Left_Sub_Tree, Right_Sub_Tree : Messages.Tree   := Empty_Tree;
      Comparison_Result             : Comparison_Results;

   begin -- Compare
      New_Line;
      Put (Left);
      Put (Right);
      if Left (L_Cc) /= Empty or Right (R_Cc) /= Empty then
         raise Compare_Error with "Tree root not link only element";
      end if; -- Left (L_Cc) /= Empty or Right (R_Cc) /= Empty
      L_Cc := First_Child (L_Cc);
      R_Cc := First_Child (R_Cc);
      while L_Cc /= Messages.No_Element and R_Cc /= Messages.No_Element and
        Comparison_Result.Result and not Comparison_Result.Decided
      loop
         if Is_Leaf (L_Cc) and Is_Leaf (R_Cc) then
            Put_Line ("Both leaves");
            Comparison_Result.Result := Left (L_Cc) <= Right (R_Cc);
            Put_Line
              (Element (L_Cc)'Img & " <=" & Element (R_Cc)'Img & " " &
               Comparison_Result.Result'Img);
            -- End comparison when there is a result?
            Comparison_Result.Decided := Left (L_Cc) < Right (R_Cc);
         elsif Is_Leaf (L_Cc) then
            if Element (L_Cc) = Empty then
               Put_Line ("Left leaf and empty");
               -- Right is not a leaf and hence "greater then" an empty list.
               Comparison_Result.Result  := True;
               Comparison_Result.Decided := True;
            else
               Put_Line ("Left leaf and sub tree");
               if Next_Sibling (L_Cc) = Messages.No_Element
                 or else Element (Next_Sibling (L_Cc)) = Empty
               then
                  -- Unique value to compare with first elemnet of list
                  Clear (Left_Sub_Tree);
                  Clear (Right_Sub_Tree);
                  S_Tc := Root (Left_Sub_Tree);
                  Append_Child (Left_Sub_Tree, S_Tc, Empty);
                  S_Tc := First_Child (S_Tc);
                  Append_Child (Left_Sub_Tree, S_Tc, Element (L_Cc));
                  Copy_Subtree
                    (Right_Sub_Tree, Root (Right_Sub_Tree),
                     Messages.No_Element, R_Cc);
                  Comparison_Result := Compare (Left_Sub_Tree, Right_Sub_Tree);
               else
                  -- Left ran out first
                  Comparison_Result.Result  := True;
                  Comparison_Result.Decided := True;
               end if; -- Next_Sibling (L_Cc) = Messages.No_Element
            end if; -- Element (L_Cc) = Empty
         elsif Is_Leaf (R_Cc) then
            if Element (R_Cc) = Empty then
               Put_Line ("Right leaf and empty");
               -- Right is an empty list and left is a list, right ran out
               -- first.
               Comparison_Result.Result  := False;
               Comparison_Result.Decided := True;
            else
               Put_Line ("Right leaf and sub tree");
               if Next_Sibling (R_Cc) = Messages.No_Element
                 or else Element (Next_Sibling (R_Cc)) = Empty
               then
                  -- Unique value to compare to list
                  Clear (Left_Sub_Tree);
                  Clear (Right_Sub_Tree);
                  Copy_Subtree
                    (Left_Sub_Tree, Root (Left_Sub_Tree), Messages.No_Element,
                     L_Cc);
                  S_Tc := Root (Right_Sub_Tree);
                  Append_Child (Right_Sub_Tree, S_Tc, Empty);
                  S_Tc := First_Child (S_Tc);
                  Append_Child (Right_Sub_Tree, S_Tc, Element (R_Cc));
                  Comparison_Result := Compare (Left_Sub_Tree, Right_Sub_Tree);
               else
                  -- Right ran out first
                  Comparison_Result.Result  := False;
                  Comparison_Result.Decided := True;
               end if; -- Next_Sibling (R_Cc) = Messages.No_Element
            end if; -- Element (E_Cc) = Empty
         else
            -- Both are sub trees
            Put_Line ("Both are sub trees");
            Clear (Left_Sub_Tree);
            Clear (Right_Sub_Tree);
            Copy_Subtree
              (Left_Sub_Tree, Root (Left_Sub_Tree), Messages.No_Element, L_Cc);
            Copy_Subtree
              (Right_Sub_Tree, Root (Right_Sub_Tree), Messages.No_Element,
               R_Cc);
            Comparison_Result := Compare (Left_Sub_Tree, Right_Sub_Tree);
         end if; -- Is_Leaf (L) and Is_Leaf (R_Cc)
         Next_Sibling (L_Cc);
         Next_Sibling (R_Cc);
      end loop; -- L_Cc /= Messages.No_Element  and R_Cc /= Messages.No_Element
      -- Fail if Left runs out first
      Comparison_Result.Result :=
        Comparison_Result.Result and
        (Comparison_Result.Decided or L_Cc = Messages.No_Element);
      Put_Line ("return" & Comparison_Result'Img);
      return Comparison_Result;
   end Compare;

   Message_Store : Message_Stores.Vector;
   Sum           : Natural := 0;

begin -- December_13
   Read_Input (Message_Store);
   for M in Iterate (Message_Store) loop
      New_Line;
      Put_Line ("Pair:" & To_Index (M)'Img);
      if Compare (Message_Store (M).Left, Message_Store (M).Right).Result then
         New_Line;
         Put_Line ("Pair:" & To_Index (M)'Img & " Correct");
         Sum := Sum + To_Index (M);
      else
         New_Line;
         Put_Line ("Pair:" & To_Index (M)'Img & " Wrong");
      end if; -- Compare (Message_Store (M). Left, Message_Store(M).Right) ...
   end loop; -- M in Iterate (Message_Store)
   Put_Line ("Part one:" & Sum'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_13;
