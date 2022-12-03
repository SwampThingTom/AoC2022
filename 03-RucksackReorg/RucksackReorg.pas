{ Rucksack Reorganization
  https://adventofcode.com/2022/day/3 
  
  Part 1
  Find the common letter in both halves of a string.
  Sum their numerical values across all strings. }

Program RucksackReorg;

Uses StrUtils;


Type
  TRucksack = string;
  TRucksacks = array of TRucksack;
  TCompartment = set of char;


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


{ Return a set of characters containing all of the characters in the 
  compartment string. }
Function CompartmentSet(compartment : string) : TCompartment;
Var
  ch : char;
Begin
  CompartmentSet := [];
  For ch In compartment Do
    CompartmentSet := CompartmentSet + [ch];
End;


{ Return the first item in the compartment.

  Set handling in Pascal appears to be quite limited. This is a bit of a hack
  since I couldn't quickly find a way to get an iterator for a set and get the
  next (first) item. }
Function FirstItem(compartment: TCompartment) : char;
Var
  ch : char;
Begin
  For ch In compartment Do
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
  leftCompartment : TCompartment;
  rightCompartment : TCompartment;
  common : TCompartment;
Begin
  compartmentLength := Trunc(Length(sack)/2);
  leftCompartment := CompartmentSet(LeftStr(sack, compartmentLength));
  rightCompartment := CompartmentSet(RightStr(sack, compartmentLength));

  common := leftCompartment * rightCompartment;
  Priority := ItemPriority(FirstItem(common));
End;


{ Returns the sum of the priorities of all rucksacks. }
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


Var 
  rucksacks : TRucksacks;
  priorities : integer;

Begin
  rucksacks := GetRucksacks();
  priorities := SumOfPriorities(rucksacks);
  WriteLn('Part 1: ', priorities);
End.
