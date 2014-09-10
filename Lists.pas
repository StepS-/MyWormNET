unit Lists;

{-------------------------------------------------------------------------------
| White/banlists unit.
| (C) StepS - 2014
|-------------------------------------------------------------------------------
| FEATURES:
|
| - Hold the lists and provide interface for their usage
| - Get and update lists in files
-------------------------------------------------------------------------------}

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  Classes, DateUtils;

type
  TListEntry = class(TObject)
    private
      SetAt: TDateTime;
      Duration: Integer;
      function HasExpired: Boolean;

    public
      Name, SetBy, Comment: string;
      Permanent: Boolean;
      Relatives: TStringList;
      property Expired: Boolean read HasExpired;
    end;

var
  WhitePassauthThreadList: TThreadList;
  WhiteNickThreadList: TThreadList;
  WhiteIPThreadList: TThreadList;
  IPBanThreadList: TThreadList;
  NickBanThreadList: TThreadList;

procedure GetListFromFile(var ThreadList: TThreadList; FileName: string);
procedure UpdateListInFile(var ThreadList: TThreadList; FileName: string);
procedure RemoveListEntry(var ThreadList: TThreadList; Name: String);
function InList(var ThreadList: TThreadList; Name: String): Boolean;
function ValidateRelativity(var ThreadList: TThreadList; Name, Relative: String): Boolean;
function RelativeExists(var ThreadList: TThreadList; Relative: String): Boolean;
procedure FreeObjectsFromThreadList(var ThreadList: TThreadList);

function NewListEntry(Name, SetBy, Comment: string; Permanent: Boolean; SetAt: TDateTime; Duration: Integer): TListEntry;

implementation

uses
  Types, SysUtils, Data, Base;

procedure GetListFromFile(var ThreadList: TThreadList; FileName: string);
var
  Buf, Line: string;
  ListEntry: TListEntry;
begin
  FreeObjectsFromThreadList(ThreadList);
  Buf:=GetTextFile(FileName);
  while GetLine(Buf,Line) do
  begin
    Line:=Trim(Line);
    if Line <> '' then
    begin
      if Pos(' ', Line) <> 0 then
      begin
        ListEntry:=NewListEntry(StringSection(Line, 0), ServerHost, 'Server-listed', true, Now, -1);
        Line:=ContinuedSection(Line, 1)+',';
        while Pos(',', Line) <> 0 do
        begin
          ListEntry.Relatives.Add(Trim(Copy(Line, 1, Pos(',',Line)-1)));
          Delete(Line, 1, Pos(',',Line));
        end;
        ThreadList.Add(ListEntry);
      end
      else
        ThreadList.Add(NewListEntry(StringSection(Line, 0), ServerHost, 'Server-listed', true, Now, -1));
    end;
  end;
end;

procedure UpdateListInFile(var ThreadList: TThreadList; FileName: string);
var
  I: Integer;
  F: text;
  List: TList;
begin
  {$I-}
  List:=ThreadList.LockList;
  ForceDirectoriesEx(FileName);
  AssignFile(F, FileName);
  Rewrite(F);
  for I:=0 to List.Count-1 do
    WriteLn(F, TListEntry(List[I]).Name);
  CloseFile(F);
  ThreadList.UnlockList;
  {$I+}
end;

function InList(var ThreadList: TThreadList; Name: String): Boolean;
var
  I: Integer;
  List: TList;
begin
  Result:=false;
  List:=ThreadList.LockList;
  for I:=0 to List.Count-1 do
    if TextMatch(TListEntry(List[I]).Name, Name) then
    begin
      if not TListEntry(List[I]).Expired then
        Result := true;
      Break;
    end;
  ThreadList.UnlockList;
end;

function ValidateRelativity(var ThreadList: TThreadList; Name, Relative: String): Boolean;
var
  I, J: Integer;
  Found, Checked: Boolean;
  ListEntry: TListEntry;
  List: TList;
begin
  Result:=true;
  Found:=false;
  Checked:=false;
  List:=ThreadList.LockList;
  for I := 0 to List.Count-1 do
  begin
    ListEntry:=TListEntry(List[I]);
    if TextMatch(ListEntry.Name, Name) then
    begin
      Found:=true;
      for J := 0 to ListEntry.Relatives.Count-1 do
        if TextMatch(ListEntry.Relatives[J], Relative) then
        begin
          Checked:=true;
          Break
        end;
    end;
  end;
  ThreadList.UnlockList;
  if not Found or Checked then
    Result:=true
  else if Found and not Checked then
    Result:=false;
end;

function RelativeExists(var ThreadList: TThreadList; Relative: String): Boolean;
var
  I, J: Integer;
  ListEntry: TListEntry;
  List: TList;
begin
  Result:=false;
  List:=ThreadList.LockList;
  for I := 0 to List.Count-1 do
  begin
    ListEntry:=TListEntry(List[I]);
    for J := 0 to ListEntry.Relatives.Count-1 do
      if TextMatch(ListEntry.Relatives[J], Relative) then
      begin
        Result:=true;
        Break
      end;
  end;
  ThreadList.UnlockList;
end;

procedure RemoveListEntry(var ThreadList: TThreadList; Name: String);
var
  I: Integer;
  List: TList;
begin
  List:=ThreadList.LockList;
  for I:=List.Count-1 downto 0 do
    if TextMatch(TListEntry(List[I]).Name, Name) then
    begin
      List.Remove(List[I]);
      TListEntry(List[I]).Free;
      Break;
    end;
  ThreadList.UnlockList;
end;

function NewListEntry(Name, SetBy, Comment: string; Permanent: Boolean; SetAt: TDateTime; Duration: Integer): TListEntry;
begin
  Result:=TListEntry.Create;
  Result.Name:=Name;
  Result.SetBy:=SetBy;
  Result.Comment:=Comment;
  Result.Permanent:=Permanent;
  Result.SetAt:=SetAt;
  Result.Duration:=Duration;
  Result.Relatives:=TStringList.Create;
end;

procedure FreeObjectsFromThreadList(var ThreadList: TThreadList);
var
  I: Integer;
  List: TList;
begin
  List:=ThreadList.LockList;
  for I := 0 to List.Count-1 do
    TObject(List[I]).Free;
  List.Clear;
  ThreadList.UnlockList;
end;

function TListEntry.HasExpired: Boolean;
begin
  Result:=false;
  if not Permanent then
    if SecondsBetween(Now, SetAt) > Duration then
       Result:=true;
end;


initialization
  WhitePassauthThreadList:=TThreadList.Create;
  WhiteNickThreadList:=TThreadList.Create;
  WhiteIPThreadList:=TThreadList.Create;
  IPBanThreadList:=TThreadList.Create;
  NickBanThreadList:=TThreadList.Create;

finalization
  WhitePassauthThreadList.Free;
  WhiteNickThreadList.Free;
  WhiteIPThreadList.Free;
  IPBanThreadList.Free;
  NickBanThreadList.Free;

end.

