with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Multiway_Trees;
with Ada.Exceptions; use Ada.Exceptions;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_07 is

   type FS_Elements is record
      Size : Natural := 0;
      Name : Unbounded_String := Null_Unbounded_String;
   end record; -- FS_Elements

   function "=" (Left, Right : FS_Elements) return Boolean is

   begin -- "="
      return Left.Name = Right.Name;
   end "=";

   package File_Systems is new Ada.Containers.Multiway_Trees (FS_Elements);
   use File_Systems;

   FS_Error : exception;

   procedure Read_Input (File_System : out File_Systems.Tree)is
      Space_Set : Character_Set := To_Set (' ');
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Not_Finished : Boolean := True;
      PWD : File_Systems.Cursor := File_Systems.No_Element;
      FS_Element : FS_Elements;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_07.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (File_System);
      while not End_Of_File (Input_File) and Not_Finished loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
         if Slice (Text, First, Last) = "$" then
            -- Is Command
            Start_At := Last + 1;
            Find_Token (Text, Lower_Set, Start_At, inside, First, Last);
            if slice (Text, First, Last) = "cd" then
               Start_At := Last + 1;
               Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
               if Slice (Text, First, Last) = "/" then
                  -- create root directory
                  FS_Element := (0, To_Unbounded_String ("/"));
                  PWD := Root (File_System);
                  Append_Child (File_System, PWD, FS_Element);
                  PWD := First_Child (PWD);
               elsif Slice (Text, First, Last) = ".." then
                  PWD := Parent (PWD);
               elsif Last > 0 then
                  -- Descend one directory
                  for C in Iterate_Children (File_System, PWD) loop
                     if File_System (C).Name =
                       Unbounded_Slice (Text, First, Last) then
                        PWD := C;
                     end if; -- File_System (C).Name = ...
                  end loop; -- C in Iterate_Children (File_System, PWD)
                  if File_System (PWD).Name /=
                    Unbounded_Slice (Text, First, Last) then
                     raise FS_Error with "directory " &
                       Slice (Text, First, Last) & " not found";
                  end if; -- File_System (PWD).Name /= ...
               else
                  raise FS_Error with "Missing directory name";
               end if; -- Slice (Text, First, Last) = "/"
            elsif slice (Text, First, Last) = "ls" then
               null;
            else
               raise FS_Error with "Expected ""cd"" or ""ls"" and found """
                 & slice (Text, First, Last) & '"';
            end if; -- slice (Text, First, Last) = "cd"
         else
            -- Command result
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            if Last > 0 then
               -- is a file
               FS_Element.Size := Natural'Value (Slice (Text, First, Last));
               Start_At := Last + 1;
               Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
               FS_Element.Name := Unbounded_Slice (Text, First, Last);
               Append_Child (File_System, PWD, FS_Element);
            else
               Find_Token (Text, Lower_Set, Start_At, Inside, First, Last);
               if Slice (Text, First, Last) = "dir" then
                  Start_At := Last + 1;
                  Find_Token (Text, Space_Set, Start_At, Outside, First, Last);
                  FS_Element := (0, Unbounded_Slice (Text, First, Last));
                  Append_Child (File_System, PWD, FS_Element);
               else
                  raise FS_Error with "Expevted ""dir"" or number and found """
                    & Slice (Text, First, Last) & '"';
               end if; -- Slice (Text, First, Last) = "dir"
            end if; -- Last > 0
         end if; -- Slice (Text, First, Last) = "$"
      end loop; -- not End_Of_File (Input_File) and Not_Finished
      Close (Input_File);
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   Function Sub_Tree_Size (File_System : in out File_Systems.Tree;
                           Tc : in File_Systems.Cursor) return Natural is

      -- This function has intentional side effects in that it updates the size
      -- of directories.

      Result : Natural := 0;

   begin -- Sub_Tree_Size
      if Is_Leaf (Tc) then
         Result := File_System (Tc).Size;
      else
         for I in Iterate_Children (File_System, TC) loop
            Result := Result + Sub_Tree_Size (File_System, I);
         end loop; -- I in Iterate_Subtree (Tc)
         File_System (Tc).Size := Result;
      end if; -- Leaf (Tc)
      return Result;
   end Sub_Tree_Size;

   Disc : constant Natural := 70000000;
   Free_Space : constant Natural := 30000000;
   File_System : File_Systems.Tree;
   Total, To_Be_Deleted, Smallest_Deletion : Natural;

begin -- December_07
   Read_Input (File_System);
   Total := Sub_Tree_Size (File_System,First_Child (Root (File_System)));
   To_Be_Deleted := Total - (Disc - Free_Space);
   Smallest_Deletion := Total;
   Total := 0;
   for D in Iterate (File_System) loop
      if not Is_Leaf (D) and File_System (D).Size < 100000 then
         Total := Total + File_System (D).Size;
      end if; -- not Is_Leaf (D) and File_System (D).Size < 100000
   end loop; -- in Iterate (File_System)
   Put_Line ("Part one:" & Total'Img);
   DJH.Execution_Time.Put_CPU_Time;
   for D in Iterate (File_System) loop
      if not Is_Leaf (D) and File_System (D).Size > To_Be_Deleted then
         if Smallest_Deletion > File_System (D).Size then
            Smallest_Deletion := File_System (D).Size;
         end if; -- Smallest_Deletion > File_System (D).Size
      end if; -- not Is_Leaf (D) and File_System (D).Size > To_Be_Deleted
   end loop; -- in Iterate (File_System)
   Put_Line ("Part two:" & Smallest_Deletion'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_07;
