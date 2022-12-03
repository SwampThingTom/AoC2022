{ Rucksack Reorganization
  https://adventofcode.com/2022/day/3 
  
  Part 1
  Find the common letter in both halves of a string.
  Sum their numerical values across all strings. 
  
  Part 2
  Find the common letter across three strings.
  Sum their numerical values across all groups of three strings. }

Program RucksackReorg;

Uses StrUtils;


Type
  TRucksack = string;
  TRucksacks = array of TRucksack;
  TItemSet = set of char;


{ Read rucksacks from file. Each rucksack is a string. }
Function GetRucksacks() : TRucksacks;
Var 
  inputFile : TextFile;
  count : integer;
Begin
  Assign(inputFile, 'input.txt');
  {$I-}
  Reset(inputFile);
  {$I+}
  If IOResult <> 0 Then
    Begin
      WriteLn('ERROR: Unable to open file.');
      Exit;
    End;

  { Use a reasonable default for the size of the array we'll need. }
  SetLength(GetRucksacks, 100);
  count := 0;
  While Not Eof(inputFile) Do
    Begin
      {$I-}
      ReadLn(inputFile, GetRucksacks[count]);
      {$I+}
      If IOResult <> 0 Then
        Begin
          WriteLn('ERROR: Unable to read file.');
          Close(inputFile);
          Exit;
        End;

      Inc(count);
      If count = Length(GetRucksacks) Then
        { Double the size of our array. }
        SetLength(GetRucksacks, Length(GetRucksacks) * 2);
    End;
  Close(inputFile);

  { Set the array size to the number of values actually read. }
  SetLength(GetRucksacks, count);
End;


{ Return a set of characters containing all of the characters in items. }
Function ItemSet(items : string) : TItemSet;
Var
  ch : char;
Begin
  ItemSet := [];
  For ch In items Do
    ItemSet := ItemSet + [ch];
End;


{ Return the first item in the items set.

  Set handling in Pascal appears to be quite limited. This is a bit of a hack
  since I couldn't quickly find a way to get an iterator for a set and get the
  next (first) item. }
Function FirstItem(items: TItemSet) : char;
Var
  ch : char;
Begin
  For ch In items Do
    Exit(ch);
End;


{ Return the priority of the given item. }
Function ItemPriority(item : char) : integer;
Begin
  If item In ['a'..'z'] Then
    ItemPriority := Ord(item) - Ord('a') + 1
  Else
    ItemPriority := Ord(item) - Ord('A') + 27;
End;


{ Returns the priority of the item that is common to both compartments of a 
  rucksack. }
Function Priority(sack : TRucksack) : integer;
Var
  compartmentLength : integer;
  leftCompartment : TItemSet;
  rightCompartment : TItemSet;
  common : TItemSet;
Begin
  compartmentLength := Trunc(Length(sack)/2);
  leftCompartment := ItemSet(LeftStr(sack, compartmentLength));
  rightCompartment := ItemSet(RightStr(sack, compartmentLength));

  common := leftCompartment * rightCompartment;
  Priority := ItemPriority(FirstItem(common));
End;


{ Part 1: Returns the sum of the priorities of all rucksacks. }
Function SumOfPriorities(rucksacks : TRucksacks) : integer;
Var
  sack : string;
Begin
  SumOfPriorities := 0;
  For sack In rucksacks Do
    Begin
      SumOfPriorities := SumOfPriorities + Priority(sack);
    End;
End;


{ Returns the priority of the item that is common across all three sacks. }
Function BadgeForGroup(sack1, sack2, sack3 : TRucksack) : integer;
Var
  common : TItemSet;
Begin
  common := ItemSet(sack1) * ItemSet(sack2) * ItemSet(sack3);
  BadgeForGroup := ItemPriority(FirstItem(common));
End;


{ Part 2: Returns the sum of the priorities of all groups' badges. }
Function SumOfBadges(rucksacks : TRucksacks) : integer;
Var
  sack : integer;
Begin
  SumOfBadges := 0;
  sack := 0;
  While sack < Length(rucksacks) Do
    Begin
      SumOfBadges := SumOfBadges + BadgeForGroup(
        rucksacks[sack], 
        rucksacks[sack+1], 
        rucksacks[sack+2]);
      sack := sack + 3;
    End;
End;


Var 
  rucksacks : TRucksacks;
  result : integer;

Begin
  rucksacks := GetRucksacks();
  result := SumOfPriorities(rucksacks);
  WriteLn('Part 1: ', result);

  result := SumOfBadges(rucksacks);
  WriteLn('Part 2: ', result)
End.
