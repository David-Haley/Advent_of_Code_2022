with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

procedure December_10_Compiler is

   type Op_Codes is (AddX, Noop);

   package OpCode_IO is new Ada.Text_IO.Enumeration_IO (Op_Codes);

   Line_Length : constant Positive := 40;
   Lines : constant Positive := 6;

   subtype Clocks is Natural range 0 .. Line_Length * Lines + 2;

   type Pixel_Arrays is array (Clocks) of Boolean;

   procedure Read_Input (Pixel_Array : out Pixel_Arrays) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Clock : Clocks := 0;

   begin -- Read_Input
      Open (Input_File, In_File, Argument(1));
      Pixel_Array := (others => False);
      for L in Positive range 1 .. Lines loop
         Get_Line (Input_File, Text);
         for P in Positive range 1 .. Line_Length loop
            Pixel_Array (Clock) := Element (Text, P) = '#';
            Clock := Clock + 1;
         end loop; -- P in Positive range 1 .. Line_Length
      end loop; -- not End_Of_File (Input_File) and Not_Finished
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Code_Generator (Pixel_Array : in Pixel_Arrays) is

      type Pixel_Runs is mod 4; -- need a two pixel look ahead

      function Look_Ahead (Pixel_Array : in Pixel_Arrays;
                           Clock : Clocks) return Pixel_Runs is

      begin -- Look_Ahead
         if not Pixel_Array (Clock) and not Pixel_Array (Clock + 1) then
            return 0;
         elsif not Pixel_Array (Clock) and Pixel_Array (Clock + 1) then
            return 1;
         elsif Pixel_Array (Clock) and not Pixel_Array (Clock + 1) then
            return 2;
         else
            return 3;
         end if; -- not Pixel_Array (Clock) and not Pixel_Array (Clock + 1)
      end Look_Ahead;

      Output_File : File_Type;
      Clock : Clocks := 2;
      X_Current : Integer := 1;
      X_Write, Difference : Integer;
      Pixel_Run : Pixel_Runs := 0;

   begin -- Code_Generator
      -- NB the first two pixels will always
      Create (Output_File, Out_File, Argument (2));
      OpCode_IO.Default_Setting := Lower_Case;
      -- Location of first pixel to be written
      X_Write := Clock mod Line_Length;
      Difference := 0;
      loop -- two pixels
         X_Write := Clock mod Line_Length;
         Difference := 0;
         case Look_Ahead (Pixel_Array, Clock) is
            when 0 =>
               if X_Write < X_Current - 2 or X_Write > X_Current + 1 then
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
               else
                  OpCode_IO.Put (Output_File, AddX);
                  X_Write := X_Write - 2;
                  Difference := X_Write - X_Current;
                  X_Current := X_Current + Difference;
                  Put_Line (Output_File, ' ' & Difference'Img);
               end if; -- Rquired_X < X_Current - 2 or Rquired_X > X_Current + 1
            when 1 =>
               if X_Write = X_Current - 2 then
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
               else
                  OpCode_IO.Put (Output_File, AddX);
                  X_Write := X_Write + 2;
                  Difference := X_Write - X_Current;
                  X_Current := X_Current + Difference;
                  Put_Line (Output_File, ' ' & Difference'Img);
               end if; -- Rquired_X = X_Current - 2
            when 2 =>
               if X_Write = X_Current + 1 then
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
               else
                  OpCode_IO.Put (Output_File, AddX);
                  X_Write := X_Write - 1;
                  Difference := X_Write - X_Current;
                  X_Current := X_Current + Difference;
                  Put_Line (Output_File, ' ' & Difference'Img);
               end if; -- X_Write = X_Current + 1
            when 3 =>
               if X_Write = X_Current - 1 or X_Write = X_Current then
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
                  OpCode_IO.Put (Output_File, Noop);
                  New_Line (Output_File);
               else
                  OpCode_IO.Put (Output_File, AddX);
                  Difference := X_Write - X_Current;
                  X_Current := X_Current + Difference;
                  Put_Line (Output_File, ' ' & Difference'Img);
               end if; -- X_Write = X_Current - 1 or X_Write = X_Current
         end case; -- Look_Ahead (Pixel_Array, Clock)
         exit when Clock >= Line_Length * Lines;
         Clock := Clock + 2;
      end loop; -- two pixels
      OpCode_IO.Put (Output_File, AddX);
      X_Write := -1;
      Difference := X_Write - X_Current;
      X_Current := X_Current + Difference;
      Put_Line (Output_File, ' ' & Difference'Img);
      OpCode_IO.Put (Output_File, Noop);
      New_Line (Output_File);
      OpCode_IO.Put (Output_File, Noop);
      New_Line (Output_File);
      Close (Output_File);
   end Code_Generator;

   Pixel_Array : Pixel_Arrays;

begin -- December_10_Compiler
   if Argument_Count = 2 then
      Read_Input (Pixel_Array);
      Code_Generator (Pixel_Array);
   else
      Put_Line ("Usage: December_10_Compiler Pixel_File_Name Code_File_Name");
   end if; -- Argument_Count = 2
end December_10_Compiler;
