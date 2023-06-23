with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.maps; use Ada.Strings.maps;
with Ada.Strings.maps.Constants; use Ada.Strings.maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Vectors;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_16 is

   subtype Flowrates is Natural;

   subtype Volumes is Natural;

   subtype Rooms is String (1 .. 2);

   Starting_Room : constant Rooms := "AA";

   package Destinations is new Ada.Containers.Ordered_Sets (Rooms);
   use Destinations;

   type Valves is record
      Flowrate : Flowrates;
      Destination : Destinations.Set;
   end record;

   package Valve_Lists is new Ada.Containers.Ordered_Maps (Rooms, Valves);
   use Valve_Lists;

   subtype Times is Natural range 0 .. 30;

   Tunnel_Time : constant Times := 1;
   Opening_Time : constant Times := 1;
   Teaching_Time : constant Times := 4;

   type Tunnels is record
      Entrance, Destination : Rooms;
   end record; -- Tunnels

   function "<" (Left, Right : Tunnels) return Boolean is
     (Left.Entrance < Right.Entrance or else
        (Left.Entrance = Right.Entrance and
             Left.Destination < Right.Destination));

   package Tunnel_Uses is new
     Ada.Containers.Ordered_Sets (Tunnels);
   use Tunnel_Uses;

   -- In this context a path is a concatination of tunnels to get between
   -- valves.
   package Path_Lengths is new Ada.Containers.Ordered_Maps (Tunnels, Times);
   use Path_Lengths;

   type Search_Elements is record
      Room : Rooms;
      Path_Length : Times;
   end record; -- Search_Element

   package Queue_Interface is new
     Ada.Containers.Synchronized_Queue_Interfaces (Search_Elements);

   package Path_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interface);
   use Path_Queues;

   type Release_Keys is record
      Entrance, Destination : Rooms;
      Remaining_Time : Times;
   end record; -- Release_Keys

   function "<" (Left, Right : Release_Keys) return Boolean is
      (Left.Entrance < Right.Entrance or else
        (Left.Entrance = Right.Entrance and
              (Left.Destination < Right.Destination or else
                   (Left.Destination = Right.Destination and
                          Left.Remaining_Time < Right.Remaining_Time))));

   package Release_Tables is new
     Ada.Containers.Ordered_Maps (Release_Keys, Volumes);
   use Release_Tables;

   type Destination_Keys is record
      Entrance : Rooms;
      Remaining_Time : Times;
   end record; -- Destination_Keys

   function "<" (Left, Right : Destination_Keys) return Boolean is
     (Left.Entrance < Right.Entrance or else
        (Left.Entrance = Right.Entrance and
             Left.Remaining_Time < Right.Remaining_Time));

   package Destination_Lists is new
     Ada.Containers.Ordered_Maps (Destination_Keys, Destinations.Set);
   use Destination_Lists;

   type Operators is (Human, Elephant);

   type Operator_Elements is record
      Room : Rooms;
      Remaining : Times;
   end record; -- Operator_Elements

   type Operator_States is array (Operators) of Operator_Elements;

   type State_Elements is record
      Volume : Volumes;
      Valves_to_Open : Destinations.Set;
      Operator_State : Operator_States;
   end record; -- State_Elements

   procedure Read_Input (Valve_List : out Valve_Lists.Map) is

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Room : Rooms;
      Valve : Valves;

   begin -- Read_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_16.txt");
      else
         Open (Input_File, In_File, Argument (1));
      end if; -- Argument_Count = 0
      Clear (Valve_List);
      while not End_Of_File (Input_File) loop
         Clear (Valve.Destination);
         Get_Line (Input_File, Text);
         Start_At := 2; -- Skip uppercase V
         Find_Token (Text, Upper_Set, Start_at, Inside, First, Last);
         Room := Slice (Text, First, Last);
         Start_At := Last + 1;
         Find_Token (Text, Decimal_Digit_Set, Start_at, Inside, First, Last);
         Valve.Flowrate := Flowrates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         while Start_At < Length (Text) loop
            Find_Token (Text, Upper_Set, Start_at, Inside, First, Last);
            Include (Valve.Destination, Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- Start_At < Length (Text)
         Include (Valve_List, Room, Valve);
      end loop; -- not End_Of_File (Input_File)
   exception
      when E : others =>
         Put_Line ("Line" & Positive_Count'Image (Line (Input_File) - 1) &
                     " > " & Text);
         Put_Line (Exception_Message (E));
         raise;
   end Read_Input;

   procedure Find_Shortest_Path (Valve_List : in Valve_Lists.Map;
                                 Valves_to_Open : in Destinations.Set;
                                 Path_Length : out Path_Lengths.Map) is

      Tunnel : Tunnels;
      Current_Element, Next_Element : Search_Elements;

   begin -- Find_Shortest_Path
      Clear (Path_Length);
      -- Find all the pairs of useful valves and calculate the shortest paths
      for Start in Iterate (Valve_List) loop
         for Finish in Iterate (Valves_to_Open) loop
            if key (Start) /= Element (Finish) and
              (Contains (Valves_to_Open, Key (Start)) or
                           Key (Start) = Starting_Room) then
               Tunnel := (Entrance => Key (Start),
                          Destination => Element (Finish));
               Include (Path_Length, Tunnel, Times'Last);
            end if; -- Start /= Finish and Valve_List (Start).Flowrate > 0 ...
         end loop; -- Finish in  Iterate (Valve_List)
      end loop; -- Start in Iterate (Valve_List)
      for P in Iterate (Path_Length) loop
         declare -- new Path_Queue and Tunnel_Use for each search
            Tunnel_Use : Tunnel_Uses.Set := Tunnel_Uses.Empty_Set;
            Path_Queue : Path_Queues.Queue;
         begin
            Current_Element.Room := Key (P).Entrance;
            Current_Element.Path_Length := 0;
            Path_Queue.Enqueue (Current_Element);
            while Path_Queue.Current_Use > 0 and
              Current_Element.Room /= Key (P).Destination loop
               Path_Queue.Dequeue (Current_Element);
               for D in Iterate (Valve_List (Current_Element.Room).
                                   Destination) loop
                  Tunnel := (Current_Element.Room, Element (D));
                  Next_Element.Path_Length :=
                    Current_Element.Path_Length + Tunnel_Time;
                  if not Contains (Tunnel_Use, Tunnel) then
                     Include (Tunnel_Use, Tunnel);
                     Next_Element.Room := Element (D);
                     Path_Queue.Enqueue (Next_Element);
                  end if; -- not Contains (Tunnel_Use, Tunnel)
               end loop; --  D in Iterate (Valve_List (Current_Element.Room) ...
            end loop; -- Path_Queue.Current_Use > 0 and
         end; -- new Path_Queue and Tunnel_Use for each search
         if Current_Element.Room = Key (P).Destination then
            -- Verify loop termination was due to destination being found.
            Path_Length (P) := Current_Element.Path_Length + Opening_Time;
         end if; -- Current_Element.Room = Key (P).Destination
      end loop; -- P in Iterate (Path_Length)
   end Find_Shortest_Path;

   procedure Build_Release_Table (Valve_List : in Valve_Lists.Map;
                                  Path_Length : in Path_Lengths.Map;
                                  Release_Table : out Release_Tables.Map;
                                  Destination_List : out Destination_Lists.Map)
   is

   begin -- Build_Release_Table
      Clear (Release_Table);
      for P in Iterate (Path_Length) loop
         for T in Times range Path_Length (P) + 1 .. Times'Last loop
            include (Release_Table,
                     (Key (P).Entrance, Key (P).Destination, T),
                     (T - Path_Length (P)) *
                       Valve_List (Key (P).Destination).Flowrate);
         end loop; -- T in Times range Path_Length (P) + 1 .. Times'Last
      end loop; -- P in Iterate (Path_Length)
      Clear (Destination_List);
      -- Since the Release table only includes non zero releases the
      -- Destination in the key is useful.
      for R in Iterate (Release_Table) loop
         if not Contains (Destination_List,
                          (Key (R).Entrance, Key (R).Remaining_Time)) then
            include (Destination_List,
                     (Key (R).Entrance, Key (R).Remaining_Time),
                     Destinations.Empty_Set);
         end if; -- not Contains (Destination_List, ...
         Include (Destination_List ((Key (R).Entrance, Key (R).Remaining_Time)),
                  Key (R).Destination);
      end loop; -- R in Iterate (Release_Table)
   end Build_Release_Table;

   procedure Set_Next (Valve_List : in Valve_Lists.Map;
                       Path_Length : in Path_Lengths.Map;
                       Release_Table : Release_Tables.Map;
                       Next_Room : in Rooms;
                       Operator : in Operators;
                       Next_Element : in out State_Elements) is

      Tunnel : constant Tunnels :=
        (Next_Element.Operator_State (Operator).Room, Next_Room);

   begin -- Set_Next
      Next_Element.Volume := @ +
        Release_Table ((Tunnel.Entrance, Tunnel.Destination,
                       Next_Element.Operator_State (Operator).Remaining));
      Exclude (Next_Element.Valves_to_Open, Next_Room);
      -- Only update the room and the remaining time for the operator who
      -- is assigned this valve.
      Next_Element.Operator_State (Operator).Room := Next_Room;
      Next_Element.Operator_State (Operator).Remaining := @
        - Path_Length (Tunnel);
   end Set_Next;

   pragma Inline_Always (Set_Next);

   procedure Search_One (Valve_List : in Valve_Lists.Map;
                         Current_Element : in State_Elements;
                         Path_Length : in Path_Lengths.Map;
                         Release_Table : Release_Tables.Map;
                         Destination_List : in Destination_Lists.Map;
                         Result : in out Volumes) is

      Next_Element : State_Elements;
      Search_List : Destinations.Set;

   begin -- Search_One
      if Contains (Destination_List,
                   ((Current_Element.Operator_State (Human).Room,
                   Current_Element.Operator_State (Human).Remaining))) then
      Search_List :=
        Intersection (Current_Element.Valves_to_Open,
                      Destination_List (
                        (Current_Element.Operator_State (Human).Room,
                        Current_Element.Operator_State (Human).Remaining)));
      else
         Search_List := Destinations.Empty_Set;
      end if; -- Contains (Destination_List, ...
      if Is_Empty (Search_List) then
         if Current_Element.Volume > Result then
            Result := Current_Element.Volume;
         end if; -- Current_Element.Volume > Result
      else
         for H in Iterate (Search_List) loop
            Next_Element := Current_Element;
            Set_Next (valve_List, Path_Length, Release_Table, Element (H),
                      Human, Next_Element);
            Search_One (Valve_List, Next_Element, Path_Length, Release_Table,
                        Destination_List, Result);
         end loop; -- Iterate (Search_List)
      end if; -- Is_Empty (Search_List)
   end Search_One;

   procedure Search_Two (Valve_List : in Valve_Lists.Map;
                         Current_Element : in State_Elements;
                         Path_Length : in Path_Lengths.Map;
                         Release_Table : in Release_Tables.Map;
                         Destination_List : in Destination_Lists.Map;
                         Result : in out Volumes) is

      Next_Element : State_Elements;
      Elephant_Search_List, Human_Search_List : Destinations.Set;

   begin -- Search_Two
      if Contains (Destination_List,
                   ((Current_Element.Operator_State (Human).Room,
                    Current_Element.Operator_State (Human).Remaining))) then
         Human_Search_List :=
           Intersection (Current_Element.Valves_to_Open,
                         Destination_List (
                           (Current_Element.Operator_State (Human).Room,
                           Current_Element.Operator_State (Human).Remaining)));
      else
         Human_Search_List := Destinations.Empty_Set;
      end if; -- Contains (Destination_List, ...
      if Contains (Destination_List,
                   ((Current_Element.Operator_State (Elephant).Room,
                    Current_Element.Operator_State (Elephant).Remaining))) then
         Elephant_Search_List :=
           Intersection (Current_Element.Valves_to_Open,
                         Destination_List (
                           (Current_Element.Operator_State (Elephant).Room,
                           Current_Element.Operator_State (Elephant).
                             Remaining)));
      else
         Elephant_Search_List := Destinations.Empty_Set;
      end if; -- Contains (Destination_List, ...
      if Is_Empty (Human_Search_List) and Is_Empty (Elephant_Search_List) then
         if Current_Element.Volume > Result then
            Result := Current_Element.Volume;
         end if; -- Current_Element.Volume > Result
      elsif Current_Element.Operator_State (Elephant).Remaining >
        Current_Element.Operator_State (Human).Remaining then
         for E in Iterate (Elephant_Search_List) loop
            Next_Element := Current_Element;
            Set_Next (valve_List, Path_Length, Release_Table,
                      Element (E), Elephant, Next_Element);
            Search_Two (Valve_List, Next_Element, Path_Length,
                        Release_Table, Destination_List, Result);
         end loop; -- E in Iterate (Elephant_Search_List)
      elsif Current_Element.Operator_State (Human).Remaining >=
        Current_Element.Operator_State (Elephant).Remaining then
         -- Human has first choice if there is a tie
         for H in Iterate (Human_Search_List) loop
            Next_Element := Current_Element;
            Set_Next (valve_List, Path_Length, Release_Table,
                      Element (H), Human, Next_Element);
            Search_Two (Valve_List, Next_Element, Path_Length,
                        Release_Table, Destination_List, Result);
         end loop; -- H in Iterate (Human_Search_List)
      end if; -- Is_Empty (Human_Search_List) and ...
   end Search_Two;

   Valve_List : Valve_Lists.Map;
   Path_Length : Path_Lengths.Map;
   Release_Table : Release_Tables.Map;
   Destination_List : Destination_Lists.Map;
   Valves_to_Open : Destinations.Set := Destinations.Empty_Set;
   Current_Element : State_Elements;
   Part_One_Result, Part_Two_Result : Volumes := 0;

begin -- December_16
   Read_Input (Valve_List);
   for V in iterate (Valve_List) loop
      if Valve_List (V).Flowrate > 0 then
         Include (Valves_to_Open, Key (V));
      end if; -- Valve_List (V).Flowrate > 0
   end loop; -- V in iterate (Valve_List)
   Find_Shortest_Path (Valve_List, Valves_to_Open, Path_Length);
   Build_Release_Table (Valve_List, Path_Length, Release_Table,
                        Destination_List);
   Current_Element := (Volume => 0,
                       Valves_to_Open => Valves_to_Open,
                       Operator_State => (Human => (Room => Starting_Room,
                                                    Remaining => Times'Last),
                                          Elephant => (Room => Starting_Room,
                                                       Remaining => 0)));
   Search_One (Valve_List, Current_Element, Path_Length, Release_Table,
               Destination_List, Part_One_Result);
   Put_Line ("Part one:" & Part_One_Result'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Current_Element := (Volume => 0,
                       Valves_to_Open => Valves_to_Open,
                       Operator_State => (Human => (Room => Starting_Room,
                                                    Remaining => Times'Last -
                                                      Teaching_Time),
                                          Elephant => (Room => Starting_Room,
                                                       Remaining => Times'Last -
                                                         Teaching_Time)));
   Search_Two (Valve_List, Current_Element, Path_Length, Release_Table,
               Destination_List, Part_Two_Result);
   Put_Line ("Part two:" & Part_Two_Result'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
