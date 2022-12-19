with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO;   use Ada.Text_IO.Unbounded_IO;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time;         use DJH.Execution_Time;

procedure December_19 is

   subtype Costs is Natural;

   type Robots is (Ore, Clay, Obsidian, Geode);

   subtype Materials is Robots;

   type BOMs is array (Materials) of Costs;

   type Workforces is array (Robots) of Natural;

   type Blueprints is array (Robots) of BOMs;

   package Blueprint_Stores is new Ada.Containers.Vectors
     (Positive, Blueprints);
   use Blueprint_Stores;

   subtype Times is Natural;

   type States is record
      Material  : BOMs;
      Workforce : Workforces;
      Time      : Times;
   end record; -- States

   Format_Error : exception;

   procedure Read_Input (Blueprint_Store : out Blueprint_Stores.Vector) is

      Input_File      : File_Type;
      Text            : Unbounded_String;
      Start_At, First : Positive;
      Last            : Natural;
      Blueprint       : Blueprints := (others => (others => 0));

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_19.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Blueprint_Store);
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         -- Minimal error checking however the blueprint number is critical
         -- and must match the index determined by when it is appended.
         if Line (Input_File) - 1 /=
           Positive_Count'Value (Slice (Text, First, Last))
         then
            raise Format_Error with "Mismatch of blueprint number";
         end if; -- Line (Input_File) - 1 /=
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Ore) (Ore) := Costs'Value (Slice (Text, First, Last));
         Start_At              := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Clay) (Ore) := Costs'Value (Slice (Text, First, Last));
         Start_At               := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Obsidian) (Ore) := Costs'Value (Slice (Text, First, Last));
         Start_At                   := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Obsidian) (Clay) :=
           Costs'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Geode) (Ore) := Costs'Value (Slice (Text, First, Last));
         Start_At                := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Blueprint (Geode) (Obsidian) :=
           Costs'Value (Slice (Text, First, Last));
         Append (Blueprint_Store, Blueprint);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when E : others =>
         Put_Line
           ("Line" & Positive_Count'Image (Line (Input_File) - 1) & " > " &
              Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Update (Blueprint   : in     Blueprints; State : in States;
                     Geode_Count : in out Natural;
                     Max_Workforce : in Workforces;
                     Time_Limit : in Times := 24) is

      function Sufficient (State : in States; Bluebrint : in Blueprints;
                           Robot : in Robots;
                           Max_Workforce : in Workforces) return Boolean is

         Result : Boolean := True;

      begin -- Sufficient
         if State.Workforce (Robot) < Max_Workforce (Robot) then
            for M in Materials loop
               Result := Result and State.Material (M) >= Bluebrint (Robot) (M);
            end loop; -- M in Materials
         else
            Result := False;
         end if; -- State.Workforce (Robot) < Max_Workforce (Robot)
         return Result;
      end Sufficient;

      function Build (State : in States;
                      Bluebrint : in Blueprints;
                      Robot : in Robots) return States is

         Next_State : States := State;

      begin -- Build
         for M in Materials loop
            Next_State.Material (M) :=
              State.Material (M) - Bluebrint (Robot) (M);
         end loop; -- M in Materials
         Next_State.Workforce (Robot) := Next_State.Workforce (Robot) + 1;
         return Next_State;
      end Build;

      procedure Collect (State : in States;
                         Next_State : in out States) is

      begin -- Collect
         for M in Materials loop
            Next_State.Material (M) :=
              Next_State.Material (M) + State.Workforce (M);
         end loop; -- M in material
         Next_State.Time := State.Time + 1;
      end Collect;

      pragma Inline_Always (Sufficient, Build, Collect);

      Next_State : States;

   begin -- Update
      if State.Time = Time_limit then
         if State.Material (Geode) > Geode_Count then
            Geode_Count := State.Material (Geode);
            Put_Line (State'Img);
         end if; -- State.Material (Geode) > Geode_Count
      else
         for R in Robots loop
            if Sufficient (State, Blueprint, R, Max_Workforce) then
               Next_State := Build (State, Blueprint, R);
               Collect (State, Next_State);
               Update (Blueprint, Next_State, Geode_Count, Max_Workforce,
                       Time_Limit);
            end if; --  Sufficient (State, Bluebrint, R)
         end loop; -- R in Robots
         Next_State := State;
         Collect (State, Next_State);
         Update (Blueprint, Next_State, Geode_Count, Max_Workforce, Time_Limit);
      end if; -- State.Time = Times_limit
   end Update;

   function Quality (Blueprint_Store : in Blueprint_Stores.Vector;
                     Part_2 : Boolean := False) return Natural is

      State : States := (Material => (others => 0),
                         Workforce => (Ore => 1, others => 0),
                         Time     => 0);
      Result      : Natural := 0;
      Geode_Count : Natural;
      Max_Workforce : Workforces;

   begin -- Quality
      -- Since only one robot can be made per time unit there is no point in
      -- more robots producing output than can be consumed.
      for B in Iterate (Blueprint_Store) loop
         Max_Workforce := (Geode => Natural'Last, others => 0);
         for Rw in Robots range Ore .. Obsidian loop
            for R in Robots loop
               if Max_Workforce (Rw) < Blueprint_Store (B) (R) (Rw) then
                  Max_Workforce (Rw) := Blueprint_Store (B) (R) (Rw);
               end if; -- Max_Workforce (Rw) < Blueprint_Store (B) (R) (Rw)
            end loop; -- R in Robots
         end loop; -- Rw in Robots range Ore .. Obsidian
         Put_Line ("Max_Workforce:" & Max_Workforce'Img);
         Geode_Count := 0;
         if Part_2 then
            Update (Blueprint_Store (B), State, Geode_Count, Max_Workforce, 32);
         else
            Update (Blueprint_Store (B), State, Geode_Count, Max_Workforce);
         end if; -- Part_2
         Result := Result + To_Index (B) * Geode_Count;
      end loop; -- B in Iterate (Bluebrint_Store)
      return Result;
   end Quality;

   Blueprint_Store : Blueprint_Stores.Vector;

begin -- December_19
   Read_Input (Blueprint_Store);
   --  Put_Line ("Part one:" & Quality (Blueprint_Store)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   -- Elephants ate blueprints
   while Last_Index (Blueprint_Store) > 3 loop
      Delete_Last (Blueprint_Store);
   end loop; -- Last_Index (Blueprint_Store)
   Put_Line ("Part two:" & Quality (Blueprint_Store, True)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_19;
