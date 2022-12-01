with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_01 is

   package Food_Stores is new Ada.Containers.Vectors (Natural, Natural);
   use Food_Stores;

   function Sum (Food_Store : in Food_Stores.Vector) return Natural is

      Food_Total : Natural := 0;

   begin -- Sum
      for F in Iterate (Food_Store) loop
         Food_Total := Food_Total + Food_Store (F);
      end loop; -- F in Iterate (Food_Store)
      return Food_Total;
   end Sum;

   function "<" (Left, Right : Food_Stores.Vector) return Boolean is

   begin -- "<"
      return Sum (Left) < Sum (Right);
   end "<";

   package Elf_Stores is new
     Ada.Containers.Vectors (Natural, Food_Stores.Vector);
   use Elf_Stores;

   package Elf_Sort is new Elf_Stores.Generic_Sorting;
   use Elf_Sort;

   procedure Read_Elvs (Elf_Store : out Elf_Stores.Vector) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Food_Store : Food_Stores.Vector;

   begin -- Read_Elvs
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_01.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      loop -- Read one elf
         Get_Line (Input_File, Text);
         Clear (Food_Store);
         while Length (Text) > 0 loop
            Append (Food_Store, Natural'Value (To_String (Text)));
            if not End_Of_File (Input_File) then
               Get_Line (Input_File, Text);
            else
               Text := Null_Unbounded_String;
            end if; -- not End_Of_File Input_File)
         end loop; -- Length (Text) > 0
         if Length (Food_Store) > 0 then
            Append (Elf_Store, Food_Store);
         end if; -- Length (Food_Store) > 0
         exit when End_Of_File (Input_File);
      end loop; -- Read one elf
      Close (Input_File);
   end Read_Elvs;

   Elf_Store : Elf_Stores.Vector;
   Three_Total : Natural := 0;

begin -- December_01
   Read_Elvs (Elf_Store);
   Sort (Elf_Store);
   Put_Line ("Part one:" & Sum (Last_Element (Elf_Store))'Img);
   DJH.Execution_Time.Put_CPU_Time;
   for E in Natural range Last_Index (Elf_Store) - 2 .. Last_Index (Elf_Store)
   loop
      Three_Total := Three_Total + Sum (Elf_Store (E));
   end loop; -- E in Natural range Last_Index (Elf_Store) - 2 ...
   Put_Line ("Part two:" & Three_Total'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_01;
