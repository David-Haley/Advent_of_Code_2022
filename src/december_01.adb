with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

procedure December_01 is

   Input_File : File_Type;
   Text : Unbounded_String;

begin -- December_01
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_01.txt");
   else
      Open (Input_File, In_File, Argument(1));
   end if; -- Argument_Count = 0
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
   end loop; -- not End_Of_Line (Input_File)
   Close (Input_File);
   Put_Line (Text);
end December_01;
