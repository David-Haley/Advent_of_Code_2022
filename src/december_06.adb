with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_06 is

   procedure Read_Input (Text : out Unbounded_String) is

      Input_File : File_Type;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_06.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Different (Text : in Unbounded_String;
                       Start_At : in Positive;
                       Sequence_Length : in Positive) return Boolean is

      Result : Boolean := True;

   begin -- Different
      for I in Positive range Start_At .. Start_At + Sequence_Length - 2 loop
         for J in Positive range I + 1 .. Start_At + Sequence_Length  - 1 loop
            Result := Result and Element (Text, I) /= Element (Text, J);
         end loop; -- J in Positive range I + 1 .. Start_At + ...
      end loop; -- I in Positive range Start_At .. Start_At + ...
      return Result;
   end Different;

   Sequence_Length_1 : constant Positive := 4;
   Sequence_Length_2 : constant Positive := 14;
   Text : Unbounded_String;
   I : Positive := 1;

begin -- December_06
   Read_Input (Text);
   loop -- on starting position
      exit when I > Length (Text) - Sequence_Length_1 + 1 or
        Different (Text, I, Sequence_Length_1);
      I := I + 1;
   end loop; -- on starting position
   if Different (Text, I, Sequence_Length_1) then
      I := I + Sequence_Length_1 - 1;
      Put_Line ("Part one:" & I'Img);
   else
      Put_Line ("Part one, start sequence not found");
   end if; --  Different (Text, I, Sequence_Length_1)
   DJH.Execution_Time.Put_CPU_Time;
   I := 1;
   loop -- on starting position
      exit when I > Length (Text) - Sequence_Length_2 + 1 or
        Different (Text, I, Sequence_Length_2);
      I := I + 1;
   end loop; -- on starting position
   if Different (Text, I, Sequence_Length_2) then
      I := I + Sequence_Length_2 - 1;
      Put_Line ("Part two:" & I'Img);
   else
      Put_Line ("Part two, start sequence not found");
   end if; --  Different (Text, I, Sequence_Length_2)
   DJH.Execution_Time.Put_CPU_Time;
end December_06;
