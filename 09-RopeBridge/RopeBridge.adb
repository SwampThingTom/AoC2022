with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Interfaces; use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

procedure RopeBridge is

   --
   -- A point that has been visited.
   --
   type Point is record
      x : Integer;
      y : Integer;
   end record;

   function Equivalent_Points(left, right: Point) return Boolean is
   begin
      return left.x = right.x and left.y = right.y;
   end Equivalent_Points;

   function Point_Hash(p: Point) return Hash_Type is
      M: constant Unsigned_32 := 64997;
      value : Unsigned_32;
   begin
      value := Unsigned_32'Mod(p.x) * M + Unsigned_32'Mod(p.y);
      return Hash_Type(value);
   end Point_Hash;

   package Point_Sets is new Hashed_Sets(
      Element_Type => Point,
      Hash => Point_Hash,
      Equivalent_Elements => Equivalent_Points);
   use Point_Sets;

   --
   -- A direction and distance to move.
   --
   type Instruction is record
      direction : Character;
      distance : Integer;
   end record;

   function Parse_Instruction(line : String) return Instruction is
      move : Instruction;
      direction : Character := line(line'First);
      distance : String := line(line'First+2 .. line'Last);
   begin
      move.direction := direction;
      move.distance := Integer'Value(distance);
      return move;
   end Parse_Instruction;

   --
   -- Move the head and tail for one instruction.
   --
   procedure Move(
      move : Instruction;
      tail_locations : in out Set;
      head : in out Point;
      tail : in out Point) is

      function Sign(x : Integer) return Integer is
      begin
         if x < 0 then return -1;
         elsif x > 0 then return 1;
         else return 0;
         end if;
      end Sign;

      procedure Move_Tail(head : in Point; tail : in out Point) is
         dx : Integer;
         dy : Integer;
         manhattan_distance : Integer;
         same_row_or_column : Boolean;
      begin
         dx := head.x - tail.x;
         dy := head.y - tail.y;
         manhattan_distance := abs(dx) + abs(dy);
         same_row_or_column := head.x = tail.x or head.y = tail.y;
         if (same_row_or_column and manhattan_distance > 1) or manhattan_distance > 2 then
            tail.x := tail.x + Sign(dx);
            tail.y := tail.y + Sign(dy);
         end if;
      end Move_Tail;

   begin
      for i in 1 .. move.distance loop
         case move.direction is
            when 'U' =>
               head.y := head.y - 1;
            when 'D' =>
               head.y := head.y + 1;
            when 'L' =>
               head.x := head.x - 1;
            when 'R' =>
               head.x := head.x + 1;
            when others =>
               raise Constraint_Error;
         end case;
         Move_Tail(head, tail);
         tail_locations.Include(tail);
      end loop;
   end Move;

   --
   -- Variables
   --
   tail_locations : Set;
   head : Point := (x => 0, y => 0);
   tail : Point := (x => 0, y => 0);
   file : File_Type;
   line : Unbounded_String;
begin
   tail_locations.Insert(tail);
   Open(file, in_file, "input.txt");
   loop
      exit when End_of_File(file);
      Get_Line(file, line);
      Move(Parse_Instruction(to_string(line)), tail_locations, head, tail);
   end loop;
   Put_Line("Part 1: " & Count_Type'Image(Length(tail_locations)));
end RopeBridge;
