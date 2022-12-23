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
with Ada.Containers.Unbounded_Priority_Queues;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_16 is

   subtype Flowrates is Natural;

   subtype Volumes is Natural;

   subtype Rooms is String (1 .. 2);

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

   type Tunnels is record
      Entrance, Destination : Rooms;
   end record; -- Tunnels

   function "<" (Left, Right : Tunnels) return Boolean is
     (Left.Entrance & Left.Destination < Right.Entrance & Right.Destination);

   function "=" (Left, Right : Tunnels) return Boolean is
     (Left.Entrance & Left.Destination = Right.Entrance & Right.Destination);

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

   function Get_Path_Length (Element : Search_Elements) return Times is
     (Element.Path_Length);

   function Shorter_First (Left, Right : Times) return Boolean is
     (Left < Right);

   package Path_Queues is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Queue_Interface,
      Queue_Priority => Times,
      Get_Priority => Get_Path_Length,
      Before => Shorter_First);
   use Path_Queues;

   type State_Elements is record
      Room : Rooms;
      Volume : Volumes;
      Tunnel_Use : Tunnel_Uses.Set;
      Valves_to_Open : Destinations.Set;
      Remaining : Times;
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
                                 Path_Length : out Path_Lengths.Map) is

      Tunnel : Tunnels;
      Current_Element, Next_Element : Search_Elements;

   begin -- Find_Shortest_Path
      Clear (Path_Length);
      -- Find all the pairs of useful valves and calculate the shortest paths
      for Start in Iterate (Valve_List) loop
         for Finish in  Iterate (Valve_List) loop
            if Start /= Finish and
              (Valve_List (Start).Flowrate > 0  or Key (Start) = "AA") and
              Valve_List (Finish).Flowrate > 0 then
               Tunnel := (Entrance => Key (Start), Destination => key (Finish));
               Include (Path_Length, Tunnel, Times'Last);
            end if; -- Start /= Finish and Valve_List (Start).Flowrate > 0 ...
         end loop; -- Finish in  Iterate (Valve_List)
      end loop; -- Start in Iterate (Valve_List)
      for P in Iterate (Path_Length) loop
         declare -- new Path_Queue and Tunnel_Use for each search
            Tunnel_Use : Tunnel_Uses.Set;
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
               end loop; --  D in Iterate (Valve_List (Current_Element.Room) ..
            end loop; -- Path_Queue.Current_Use > 0 and
         end; -- new Path_Queue and Tunnel_Use for each search
         if Current_Element.Room = Key (P).Destination then
            -- Verify loop termination was due to destination being found.
            Path_Length (P) := Current_Element.Path_Length;
         end if; -- Current_Element.Room = Key (P).Destination
      end loop; -- P in Iterate (Path_Length)
   end Find_Shortest_Path;

   procedure Search (Valve_List : in Valve_Lists.Map;
                     Current_Element : in State_Elements;
                     Path_Length : in Path_Lengths.Map;
                     Result : in out Volumes) is

      function Set_Next (Valve_List : in Valve_Lists.Map;
                         Current_Element : in State_Elements;
                         Path_Length : in Path_Lengths.Map;
                         Next_Room : in Rooms;
                         Next_Element : out State_Elements) return Boolean is

         Tunnel : constant Tunnels := (Current_Element.Room, Next_Room);
         Is_Valid : Boolean;

      begin -- Set_Next
         is_Valid := Contains (Current_Element.Valves_to_Open, Next_Room) and
           Current_Element.Remaining > Path_Length (Tunnel) + Opening_Time;
         if Is_Valid then
            Next_Element := Current_Element;
            Next_Element.Room := Next_Room;
            Include (Next_Element.Tunnel_Use, Tunnel);
            Next_Element.Remaining := Current_Element.Remaining -
              (Path_Length (Tunnel) + Opening_Time);
            Next_Element.Volume := Current_Element.Volume +
              Next_Element.Remaining * Valve_List (Next_Room).Flowrate;
            Exclude (Next_Element.Valves_to_Open, Next_Room);
         end if; -- Is_Valid
         return Is_Valid;
      end Set_Next;

      Next_Element : State_Elements;

   begin -- Search
      if Current_Element.Remaining <= Tunnel_Time + Opening_Time or
        Length (Current_Element.Valves_to_Open) = 0 then
         -- Search has ended
         if Current_Element.Volume > Result then
            Result := Current_Element.Volume;
         end if; -- Current_Element.Volume > Result
      else
         for T in iterate (Path_Length) loop
            if Key (T).Entrance = Current_Element.Room then
               if Set_Next (valve_List, Current_Element, Path_Length,
                            Key (T).Destination, Next_Element) then
                  Search (Valve_List, Next_Element, Path_Length, Result);
               end if; -- Set_Next (valve_List, Current_Element, ...
            end if; --if Key (T).Entrance + Current_Element.Room
         end loop; --  T in iterate (Path_Length)
      end if; -- Current_Elemment.Remaining <= Tunnel_Time + Opening_Time or ...
   end Search;

   Valve_List : Valve_Lists.Map;
   Path_Length : Path_Lengths.Map;
   Valves_to_Open : Destinations.Set :=  Destinations.Empty_Set;
   Current_Element : State_Elements;
   Part_One_Result : Volumes := 0;

begin -- December_16
   Read_Input (Valve_List);
   for V in iterate (Valve_List) loop
      if Valve_List (V).Flowrate > 0 then
         Include (Valves_to_Open, Key (V));
      end if; -- Valve_List (V).Flowrate > 0
   end loop; -- V in iterate (Valve_List)
   Find_Shortest_Path (Valve_List, Path_Length);
   Current_Element := (Room => "AA",
                       Volume => 0,
                       Tunnel_Use => Tunnel_Uses.Empty_Set,
                       Valves_to_Open => Valves_to_Open,
                       Remaining => 30);
   Search (Valve_List, Current_Element, Path_Length, Part_One_Result);
   Put_Line ("Part one:" & Part_One_Result'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part two:");
   DJH.Execution_Time.Put_CPU_Time;
end December_16;
