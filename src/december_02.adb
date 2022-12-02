with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_02 is

   type Choices is (Rock, Paper, Scissors);

   type Rounds is record
     Opponent, Me : Choices;
   end record; -- Rounds

   package Round_Stores is new Ada.Containers.Vectors (Positive, Rounds);
   use Round_Stores;

   procedure Read_Input (Round_Store : out Round_Stores.Vector) is

      subtype Opponent_Choices is Character range 'A' .. 'C';
      subtype Me_Choices is Character range 'X' .. 'Z';

      Input_File : File_Type;
      Text : Unbounded_String;
      Opponent_Choice : Opponent_Choices;
      Me_Choice : Me_Choices;
      Delimiter : Character;
      Round : Rounds;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_02.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Clear (Round_Store);
      while not End_Of_File (Input_File) loop
         Get (Input_File, Opponent_Choice);
         Get (Input_File, Delimiter);
         Get (Input_File, Me_Choice);
         if not End_Of_File (Input_File) then
            Skip_Line (Input_File);
         end if; -- not End_Of_File (Input_File)
         case Opponent_Choice is
            when 'A' => Round.Opponent := Rock;
            when 'B' => Round.Opponent := Paper;
            when 'C' => Round.Opponent := Scissors;
         end case; -- Opponent_Choice
         if Delimiter /= ' ' then
            raise Constraint_Error with "Expected ' ' and found '" & Delimiter &
              "'";
         end if; -- Delimiter /= ' '
         case Me_Choice is
            when 'X' => Round.Me := Rock;
            when 'Y' => Round.Me := Paper;
            when 'Z' => Round.Me := Scissors;
         end case; -- Me_Choice
         Append (Round_Store, Round);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Read_Input;

   function Score_Round (Round : in Rounds) return Positive is

      function ">" (Left, Right : Choices) return Boolean is

         -- The ">" operator is defined for any enumerated type and is
         -- effectively an after operator, thus by default in both Paper and
         -- Scissors are both greater than Rock. In this case the ">" has been
         -- overloaded to be a defeats operator.

      begin -- ">"
         case Left is
            when Rock =>
               return Right = Scissors;
            when Paper =>
               return Right = Rock;
            when Scissors =>
               return Right = Paper;
         end case; -- Left
      end ">";

   begin -- Score_Round
      if Round.Me > Round.Opponent then
         return Choices'Pos (Round.Me) + 1 + 6;
      elsif Round.Me = Round.Opponent then
         return Choices'Pos (Round.Me) + 1 + 3;
      else
         return Choices'Pos (Round.Me) + 1;
      end if; -- Round.Me > Round.Opponent
   end Score_Round;

   procedure Correct (Round_Store : in out Round_Stores.Vector) is

      -- This procedure corrects the interpretation of the input as follows:
      -- input   Me         result
      --     X   Rock       Lose
      --     Y   Paper      Draw
      --     Z   Scissors   Win
      -- The Me value after correction ensures the result as above.

   begin -- Correct
      for R in Iterate (Round_Store) loop
         case Round_Store (R).Me is
            when Rock =>
               -- Lose
               case Round_Store (R).Opponent is
                  when Rock =>
                     Round_Store (R).Me := Scissors;
                  when Paper =>
                     Round_Store (R).Me := Rock;
                  when Scissors =>
                     Round_Store (R).Me := Paper;
               end case; -- Round_Store (R).Opponent
            when Paper =>
               -- Draw
               Round_Store (R).Me := Round_Store (R).Opponent;
            when Scissors =>
               -- Win
               case Round_Store (R).Opponent is
                  when Rock =>
                     Round_Store (R).Me := Paper;
                  when Paper =>
                     Round_Store (R).Me := Scissors;
                  when Scissors =>
                     Round_Store (R).Me := Rock;
               end case; -- Round_Store (R).Opponent
         end case; -- Round_Store (R).Me
      end loop; -- R in Iterate (Round_Store)
   end Correct;

   Round_Store : Round_Stores.Vector;
   Total_Score : Natural;

begin -- December_01
   Read_Input (Round_Store);
   Total_Score := 0;
   for R in Iterate (Round_Store) loop
      Total_Score := Total_Score + Score_Round (Round_Store (R));
   end loop; -- R in Iterate (Round_Store)
   Put_Line ("Part one:" & Total_Score'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Correct (Round_Store);
   Total_Score := 0;
   for R in Iterate (Round_Store) loop
      Total_Score := Total_Score + Score_Round (Round_Store (R));
   end loop; -- R in Iterate (Round_Store)
   Put_Line ("Part two:" & Total_Score'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_02;
