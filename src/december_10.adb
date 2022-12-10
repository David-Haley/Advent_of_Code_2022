with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use  Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_10 is

   type Op_Codes is (AddX, Noop);

   type Instructions (Op_Code : Op_Codes) is record
      case Op_Code is
         when AddX =>
            Operand : Integer;
         when others =>
            null;
      end case; -- Op_Code
   end record; -- Instructions

   subtype Instruction_Pointers is Natural;

   package Program_Stores is new
     Ada.Containers.Indefinite_Vectors (Instruction_Pointers, Instructions);
   use Program_Stores;

   type CPU_States is record
      IP : Instruction_Pointers := 0;
      X : Integer := 1;
      Fetch_Cycle : Boolean := True;
   end record; -- CPU_States;

   procedure Read_Input (Program_Store : out Program_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Op_Code : Op_Codes;
      Start_At, First : Positive;
      Last : Natural;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_10.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Program_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
         Op_Code := Op_Codes'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         declare
            Instruction : Instructions (Op_Code);
         begin -- declare Instruction
            if Op_Code = AddX then
               Instruction.Operand :=
                 Integer'Value (Slice (Text, Start_At, Length (Text)));
            end if; -- Op_Code = AddX
            Append (Program_Store, Instruction);
         end; -- declare Instruction
      end loop; -- not End_Of_File (Input_File) and Not_Finished
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   Procedure Cycle (Program_Store : in Program_Stores.Vector;
                    CPU_State : in out CPU_States) is

   begin -- Cycle
      if CPU_State.Fetch_Cycle then
         if Program_Store (CPU_State.IP).Op_Code = Noop then
            CPU_State.IP := CPU_State.IP + 1;
         else
            CPU_State.Fetch_Cycle := False;
         end if; -- Program_Store (CPU_State.IP).Op_Code = Noop
      else
         case Program_Store (CPU_State.IP).Op_Code is
            when AddX =>
               CPU_State.X := CPU_State.X +
                 Program_Store (CPU_State.IP).Operand;
               CPU_State.Fetch_Cycle := True;
               CPU_State.IP := CPU_State.IP + 1;
            when others =>
               raise Program_Error with "Illegal instruction Trap IP:" &
                 CPU_State.IP'Img;
         end case; -- Program_Store (CPU_State.IP).Op_Code
      end if; -- CPU_State.Fetch_Cycle
   end Cycle;

   Line_Length : constant Positive := 40;
   Lines : constant Positive := 6;
   Program_Store : Program_Stores.Vector;
   CPU_State : CPU_States;
   Sum_X : Integer := 0;

begin -- December_10
   Read_Input (Program_Store);
   Put_Line ("Part 2 answer below");
   New_line;
   For Clock in Positive range 1 .. Lines * Line_Length loop
      if (Clock + 20) mod Line_Length = 0 then
         Sum_X := Sum_X + (Clock * CPU_State.X);
      end if; -- Clock in Positive range 1 .. Lines * Line_Length
      if CPU_State.X - 1 <= (Clock - 1) mod Line_Length and
        (Clock - 1) mod Line_Length <= CPU_State.X + 1 then
         -- Sprite coincides with scan position
         Put ('#');
      else
         Put (' ');
      end if; -- CPU_State.X - 1 <= Clock mod Line_Length and
      if Clock mod Line_Length = 0 then
         New_Line;
      end if; -- Clock mod Line_Length = 0
      Cycle (Program_Store, CPU_State);
   end loop; -- Clock in Positive range 1 .. Lines * Line_Length
   New_Line;
   Put_Line ("Part one:" & Sum_X'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_10;
