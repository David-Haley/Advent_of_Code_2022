with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_21 is

   subtype Monkey_Names is String (1 .. 4);

   Root_Name : constant Monkey_Names := "root";

   To_Find : constant Monkey_Names := "humn";

   type Jobs is (Operand, Evaluation);

   subtype Operators is Character with
     Static_Predicate => Operators in '+' | '-' | '*' | '/';

   subtype Values is Long_Long_Integer;

   type Monkeys (Job : Jobs) is record
      case Job is
         when Operand =>
            Value : Values;
         when Evaluation =>
            Left, Right : Monkey_Names;
            Operation : Operators;
         end case; -- Job
   end record; -- Monkeys

   package Monkey_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Monkey_Names, Monkeys);
   use Monkey_Maps;

   type Path_Elements is record
      Operand_Name : Monkey_Names;
      Is_Left : Boolean;
   end record; -- Path_Elements

   package Paths is new Ada.Containers.Vectors (Positive, Path_Elements);
   use Paths;

   type Search_Elements is record
      Current_Result : Monkey_Names;
      Path : Paths.Vector;
   end record; -- Search_Elements

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);

   package Path_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
   use Path_Queues;

   procedure Read_Input (Monkey_Map : out Monkey_Maps.Map)is

      Operator_Set : constant Character_Set := To_Set ("+-*/");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First, End_of_Name: Positive;
      Last : Natural;
      Monkey_Name : Monkey_Names;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_21.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Monkey_Map);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
         Monkey_Name := Slice (Text, First, Last);
         Start_At := Last + 1;
         End_of_Name := Start_At;
         Find_Token (Text, Operator_Set, Start_At, Inside, First, Last);
         if Last > 0 then
            -- Evaluation monkey
            declare -- Evaluation monkey
               Monkey : Monkeys (Evaluation);
            begin
               Monkey.Operation := Element (Text, First);
               Start_At := End_of_Name;
               Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
               Monkey.Left := Slice (Text, First, Last);
               Start_At := Last + 1;
               Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
               Monkey.Right := Slice (Text, First, Last);
               Include (Monkey_Map, Monkey_Name, Monkey);
            end; -- Evaluation monkey
         else
            declare -- Operand monkey
               Monkey : Monkeys (Operand);
            begin
               Start_At := End_of_Name;
               Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               Monkey.Value := Values'Value (Slice (Text, First, Last));
               Include (Monkey_Map, Monkey_Name, Monkey);
            end; -- Operand monkey
         end if; -- Last > 0
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   function Evaluate (Monkey_Map : in Monkey_Maps.Map;
                      Monkey_Name : in Monkey_Names) return Values is

   begin -- Evaluate
      if Monkey_Map (Monkey_Name).Job = Operand then
         return Monkey_Map (Monkey_Name).Value;
      else
         case Monkey_Map (Monkey_Name).Operation is
            when '+' =>
               return Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Left) +
                 Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Right);
            when '-' =>
               return Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Left) -
                 Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Right);
            when '*' =>
               return Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Left) *
                 Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Right);
            when '/' =>
               return Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Left) /
                 Evaluate (Monkey_Map, Monkey_Map (Monkey_Name).Right);
         end case; -- Monkey_Map (Monkey_Name).Operation
      end if; -- Monkey_Map (Monkey_Name).Job = Operand
   exception
      when E : others =>
         Put_Line (Monkey_Name & ": " & Exception_Message (E));
         raise;
   end Evaluate;

   function Solve (Monkey_Map : in Monkey_Maps.Map;
                   Root_Name, To_Find : in Monkey_Names) return Values is

      function Find_Path (Monkey_Map : in Monkey_Maps.Map;
                          Root_Name, To_Find : in Monkey_Names)
                          return Paths.Vector is

         Path : Paths.Vector;
         Current_Element, Next_Element : Search_Elements;
         Path_Element : Path_Elements;
         Path_Queue : Path_Queues.Queue;

      begin -- Find_Path
         Clear (Current_Element.Path);
         Current_Element.Current_Result := Root_Name;
         Path_Queue.Enqueue (Current_Element);
         while Path_Queue.Current_Use > 0 and
           Current_Element.Current_Result /= To_Find loop
            Path_Queue.Dequeue (Current_Element);
            if Monkey_Map (Current_Element.Current_Result).Job = Evaluation then
               Next_Element.Current_Result :=
                 Monkey_Map (Current_Element.Current_Result).Left;
               Next_Element.Path := Copy (Current_Element.Path);
               Path_Element.Operand_Name := Next_Element.Current_Result;
               Path_Element.Is_Left := True;
               Append (Next_Element.Path, Path_Element);
               Path_Queue.Enqueue (Next_Element);
               Next_Element.Current_Result :=
                 Monkey_Map (Current_Element.Current_Result).Right;
               Next_Element.Path := Copy (Current_Element.Path);
               Path_Element.Operand_Name := Next_Element.Current_Result;
               Path_Element.Is_Left := False;
               Append (Next_Element.Path, Path_Element);
               Path_Queue.Enqueue (Next_Element);
            end if; -- Monkey_Map (Current_Element.Current_Reult).Job = ...
         end loop; -- Path_Queue.Current_Use > 0 and
         return Current_Element.Path;
      end Find_Path;

      Path : Paths.Vector;
      To_Match : Values;
      Result_Name : Monkey_Names;
      Pc : Paths.Cursor;
      Constant_Operand : Values;

   begin -- Solve
      Path := Find_Path (Monkey_Map, Root_Name, To_Find);
      Pc := First (Path);
      if Path (Pc).Is_Left then
         Result_Name := Monkey_Map (Root_Name).Right;
      else
         Result_Name := Monkey_Map (Root_Name).Left;
      end if; -- Path (Pc).Is_Left
      To_Match := Evaluate (Monkey_Map, Result_Name);
      Result_Name := Path (Pc).Operand_Name;
      while Result_Name /= To_Find loop
         -- Undo arithenatic
         if Path (Next (Pc)).Is_Left then
            Constant_Operand := Evaluate (Monkey_Map, Monkey_Map (Result_Name).
                                            Right);
         else
            Constant_Operand := Evaluate (Monkey_Map, Monkey_Map (Result_Name).
                                            Left);
         end if; -- Path (Pc).Is_Left
         case Monkey_Map (Result_Name).Operation is
            when '+' =>
               To_Match := To_Match - Constant_Operand;
            when '-' =>
               if Path (Next (Pc)).Is_Left then
                  To_Match := To_Match + Constant_Operand;
               else
                  To_Match := Constant_Operand - To_Match;
               end if; -- Path (Next (Pc)).Is_Left
            when '*' =>
               To_Match := To_Match / Constant_Operand;
            when '/' =>
               if Path (Next (Pc)).Is_Left then
                  To_Match := To_Match * Constant_Operand;
               else
                  To_Match := Constant_Operand / To_Match;
               end if; -- Path (Next (Pc)).Is_Left
         end case; -- Monkey_Map (Result_Name).Operation
         Next (Pc);
         Result_Name := Path (Pc).Operand_Name;
      end loop; -- Result_Name /= To_Find
      return To_Match;
   end Solve;

   Monkey_Map : Monkey_Maps.Map;

begin -- December_21
   Read_Input (Monkey_Map);
   Put_Line ("Part one:" & Evaluate (Monkey_Map, Root_Name)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:" & Solve (Monkey_Map, Root_Name, To_Find)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_21;
