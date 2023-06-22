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

   type Path_Log_Elements is record
      Time : Times;
      Operator : Operators;
      Room : Rooms;
      Volume : Volumes;
   end record;

   package Path_Logs is new
     Ada.Containers.Vectors (Positive, Path_Log_Elements);
   use Path_Logs;

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
                                  Release_Table : out Release_Tables.Map) is

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
   end Build_Release_Table;

   function Shortest_Remaining_Path (Current_Element : in State_Elements;
                                     Path_Length : in Path_Lengths.Map;
                                     Operator : in Operators)
                                        return Times is

      Result : Times := Times'Last;

   begin -- Shortest_Remaining_Path
      for V in Iterate (Current_Element.Valves_to_Open) loop
         if Result >
           Path_Length ((Current_Element.Operator_State (Operator).Room,
                        Element (V))) then
            Result :=
              Path_Length ((Current_Element.Operator_State (Operator).Room,
                           Element (V)));
         end if; -- Result > Path_Length (Element (V))
      end loop; -- V in Iterate (Current_Element.Valves_to_Open)
      return Result;
   end Shortest_Remaining_Path;

   function Is_Valid (Release_Table : Release_Tables.Map;
                      Next_Room : in Rooms;
                      Operator : in Operators;
                      Next_Element : in out State_Elements) return Boolean is
     (Contains (Release_Table,
      (Next_Element.Operator_State (Operator).Room, Next_Room,
       Next_Element.Operator_State (Operator).Remaining)));

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

   pragma Inline_Always (Is_Valid, Set_Next);

   procedure Search_One (Valve_List : in Valve_Lists.Map;
                         Current_Element : in State_Elements;
                         Path_Length : in Path_Lengths.Map;
                         Release_Table : Release_Tables.Map;
                         Result : in out Volumes) is

      Next_Element : State_Elements;

   begin -- Search_One
      if (Current_Element.Operator_State (Human).Remaining <=
            Shortest_Remaining_Path (Current_Element, Path_Length, Human) and
            Current_Element.Operator_State (Elephant).Remaining <=
            Shortest_Remaining_Path (Current_Element, Path_Length, Elephant))
        or Is_Empty (Current_Element.Valves_to_Open) then
         -- Search ended either no time to operate more valves or no more valves
         -- to operate.
         if Current_Element.Volume > Result then
            Result := Current_Element.Volume;
         end if; -- Current_Element.Volume > Result
      else
         for H in Iterate (Current_Element.Valves_to_Open) loop
            Next_Element := Current_Element;
            if Is_Valid (Release_Table, Element (H), Human, Next_Element) then
              Set_Next (valve_List, Path_Length, Release_Table, Element (H),
                         Human, Next_Element);
               Search_One (Valve_List, Next_Element, Path_Length, Release_Table,
                           Result);
            end if; -- Is_Valid (Release_Table, Element (H), Human, ...
         end loop; -- H in Iterate (Current_Element.Valves_to_Open)
      end if; -- Current_Elemment.Remaining <= Tunnel_Time + Opening_Time or ...
   end Search_One;

   procedure Search_Two (Valve_List : in Valve_Lists.Map;
                         Current_Element : in State_Elements;
                         Path_Length : in Path_Lengths.Map;
                         Release_Table : Release_Tables.Map;
                         Result : in out Volumes;
                         Path_Log : In out Path_Logs.Vector) is

      Next_Element : State_Elements;
      Path_Log_Element : Path_Log_Elements;
      Valves_Operated : Natural;

   begin -- Search_Two
      if (Current_Element.Operator_State (Human).Remaining <=
            Shortest_Remaining_Path (Current_Element, Path_Length, Human) and
              Current_Element.Operator_State (Elephant).Remaining <=
            Shortest_Remaining_Path (Current_Element, Path_Length, Elephant))
        or Is_Empty (Current_Element.Valves_to_Open) then
         -- Search ended either no time to operate more valves or no more valves
         -- to operate.
         if Current_Element.Volume >= Result then
            Result := Current_Element.Volume;
            Put_Line (Result'Img);
            for L in Iterate (Path_Log) loop
               Put_Line (Times'Image (Times'Last - Teaching_Time - Element (L).Time) & " " & Element (L).Operator'Img & " " & Element (L).Room & Element (L).Volume'Img);
            end loop;
            New_Line;
         end if; -- Current_Element.Volume > Result
      elsif Current_Element.Operator_State (Elephant).Remaining >
        Current_Element.Operator_State (Human).Remaining then
         for E in Iterate (Current_Element.Valves_to_Open) loop
            Next_Element := Current_Element;
            if Is_Valid (Release_Table, Element (E), Elephant,
                         Next_Element) then
               Set_Next (valve_List, Path_Length, Release_Table,
                         Element (E), Elephant, Next_Element);
               Path_Log_Element := (Next_Element.Operator_State (Elephant).Remaining, Elephant, Next_Element.Operator_State (Elephant).Room, Next_Element.Volume);
               Append (Path_Log, Path_Log_Element);
               Search_Two (Valve_List, Next_Element, Path_Length,
                           Release_Table, Result, Path_Log);
               Delete_Last (Path_Log);
            else
               for H in Iterate (Current_Element.Valves_to_Open) loop
                  Next_Element := Current_Element;
                  if Is_Valid (Release_Table, Element (H), Human,
                               Next_Element) then
                     Set_Next (valve_List, Path_Length, Release_Table,
                               Element (H), Human, Next_Element);
                     Path_Log_Element := (Next_Element.Operator_State (Human).Remaining, Human, Next_Element.Operator_State (Human).Room, Next_Element.Volume);
                     Append (Path_Log, Path_Log_Element);
                     Search_Two (Valve_List, Next_Element, Path_Length,
                                 Release_Table, Result, Path_Log);
                     Delete_Last (Path_Log);
                  end if; -- Is_Valid (Release_Table, Element (H), Human, ...
               end loop; -- H in Iterate (Current_Element.Valves_to_Open)
            end if; -- Is_Valid (Release_Table, Element (H), Elephant, ...
         end loop; -- E in Iterate (Current_Element.Valves_to_Open)
      elsif Current_Element.Operator_State (Human).Remaining >
        Current_Element.Operator_State (Elephant).Remaining then
         for H in Iterate (Current_Element.Valves_to_Open) loop
            Next_Element := Current_Element;
            if Is_Valid (Release_Table, Element (H), Human,
                         Next_Element) then
               Set_Next (valve_List, Path_Length, Release_Table,
                         Element (H), Human, Next_Element);
               Path_Log_Element := (Next_Element.Operator_State (Human).Remaining, Human, Next_Element.Operator_State (Human).Room, Next_Element.Volume);
               Append (Path_Log, Path_Log_Element);
               Search_Two (Valve_List, Next_Element, Path_Length,
                           Release_Table, Result, Path_Log);
               Delete_Last (Path_Log);
            else
               for E in Iterate (Current_Element.Valves_to_Open) loop
                  Next_Element := Current_Element;
                  if Is_Valid (Release_Table, Element (E), Elephant,
                               Next_Element) then
                     Set_Next (valve_List, Path_Length, Release_Table,
                               Element (E), Elephant, Next_Element);
                     Path_Log_Element := (Next_Element.Operator_State (Elephant).Remaining, Elephant, Next_Element.Operator_State (Elephant).Room, Next_Element.Volume);
                     Append (Path_Log, Path_Log_Element);
                     Search_Two (Valve_List, Next_Element, Path_Length,
                                 Release_Table, Result, Path_Log);
                     Delete_Last (Path_Log);
                  end if; -- Is_Valid (Release_Table, Element (H), Elephant, ...
               end loop; -- E in Iterate (Current_Element.Valves_to_Open)
            end if; -- Is_Valid (Release_Table, Element (H), Human, ...
         end loop; -- H in Iterate (Current_Element.Valves_to_Open)
      else
         -- Equal times remaining
         for E in Iterate (Current_Element.Valves_to_Open) loop
            for H in Iterate (Current_Element.Valves_to_Open) loop
               if Element (E) /= Element (H) then
                  Next_Element := Current_Element;
                  Valves_Operated := 0;
                  if Is_Valid (Release_Table, Element (E), Elephant,
                               Next_Element) then
                     Set_Next (valve_List, Path_Length, Release_Table,
                               Element (E), Elephant, Next_Element);
                     Path_Log_Element := (Next_Element.Operator_State (Elephant).Remaining, Elephant, Next_Element.Operator_State (Elephant).Room, Next_Element.Volume);
                     Append (Path_Log, Path_Log_Element);
                     Valves_Operated := @ + 1;
                  end if; -- Is_Valid (Release_Table, Element (H), Elephant, ...
                  if Is_Valid (Release_Table, Element (H), Human,
                               Next_Element) then
                     Set_Next (valve_List, Path_Length, Release_Table,
                               Element (H), Human, Next_Element);
                     Path_Log_Element := (Next_Element.Operator_State (Human).Remaining, Human, Next_Element.Operator_State (Human).Room, Next_Element.Volume);
                     Append (Path_Log, Path_Log_Element);
                     Valves_Operated := @ + 1;
                  end if; -- Is_Valid (Release_Table, Element (H), Human, ...
                  if Valves_Operated > 0 then
                     Search_Two (Valve_List, Next_Element, Path_Length,
                                 Release_Table, Result, Path_Log);
                  end if; -- Valves_Operated > 0
                  for C in Natural range 1 .. Valves_Operated loop
                     Delete_Last (Path_Log);
                  end loop; -- C in Natural range 1 .. Valves_Operated
               end if; -- Element (E) /= Element (H)
            end loop; -- H in Iterate (Current_Element.Valves_to_Open)
         end loop; -- E in Iterate (Current_Element.Valves_to_Open)
      end if; -- (Current_Element.Operator_State (Human).Remaining <= ...
   end Search_Two;

   Valve_List : Valve_Lists.Map;
   Path_Length : Path_Lengths.Map;
   Release_Table : Release_Tables.Map;
   Valves_to_Open : Destinations.Set :=  Destinations.Empty_Set;
   Current_Element : State_Elements;
   Part_One_Result, Part_Two_Result : Volumes := 0;
   Path_Log : Path_Logs.Vector := Path_Logs.Empty_Vector;

begin -- December_16
   Read_Input (Valve_List);
   for V in iterate (Valve_List) loop
      if Valve_List (V).Flowrate > 0 then
         Include (Valves_to_Open, Key (V));
      end if; -- Valve_List (V).Flowrate > 0
   end loop; -- V in iterate (Valve_List)
   Put_Line ("Find_Shortest_Path");
   Find_Shortest_Path (Valve_List, Valves_to_Open, Path_Length);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Build_Release_Table");
   Build_Release_Table (Valve_List, Path_Length, Release_Table);
   DJH.Execution_Time.Put_CPU_Time;
   Current_Element := (Volume => 0,
                       Valves_to_Open => Valves_to_Open,
                       Operator_State => (Human => (Room => Starting_Room,
                                                    Remaining => Times'Last),
                                          Elephant => (Room => Starting_Room,
                                                       Remaining => 0)));
   Search_One (Valve_List, Current_Element, Path_Length, Release_Table,
               Part_One_Result);
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
               Part_Two_Result, Path_Log);
   Put_Line ("Part two:" & Part_Two_Result'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
