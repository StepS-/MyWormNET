unit Version;


{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

type
  UInt = LongWord;
  TVersion = class
    private
      WordArray: array of word;
      Errors: UInt;
      function GetVal(Index: UInt): Word;
      procedure SetVal(Index: UInt; const Value: Word);
      function GetSize: UInt;
      procedure SetSize(Size: UInt);
      function GetString: String;
      procedure SetString(S: String);
      function GetInt64: Int64;
      procedure SetInt64(Val64: Int64);
      function IsValid: Boolean;

    public
      property Num[Index: UInt]: word read GetVal write SetVal; default;
      property Str: string read GetString write SetString;
      property Int64: Int64 read GetInt64 write SetInt64;
      property Size: UInt read GetSize write SetSize;
      property Valid: Boolean read IsValid;

      function IsAtLeast(V: TVersion): Boolean; overload;
      function IsAtLeast(S: String): Boolean; overload;
      function IsOlderThan(V: TVersion): Boolean; overload;
      function IsOlderThan(S: String): Boolean; overload;
      function IsBetween(V1, V2: TVersion): Boolean; overload;
      function IsBetween(S1, S2: String): Boolean; overload;
      constructor Create(S: String = '');
      destructor Destroy; override;
  end;

const
  SDec = '0123456789';

function VersionNewerOrEquals(Check, Target: string): Boolean;
function VersionOlder(Check, Target: string): Boolean;
function VersionBetween(Check, S1, S2: string): Boolean;

function IsValidVerStr(Ver: String): Boolean;
function StrToVersion(Ver: String): TVersion;

function MakeLongLong(L1, L2: LongWord): Int64;
function MakeQword(W1, W2, W3, W4: Word): Int64;


implementation

uses
  SysUtils;

function TVersion.GetVal(Index: UInt): Word;
begin
  if (Index < 16) and (Index < Size) then
    Result:=WordArray[Index]
  else
    Result:=0;
end;

procedure TVersion.SetVal(Index: UInt; const Value: Word);
begin
  if Index < 16 then
  begin
    if Index+1 > Size then Size:=Index+1;
    WordArray[Index]:=Value;
  end;
end;

function TVersion.GetSize: UInt;
begin
  Result:=Length(WordArray);
end;

procedure TVersion.SetSize(Size: UInt);
begin
  if Size <= 16 then
    SetLength(WordArray, Size);
end;

function TVersion.GetString: String;
var I: UInt;
begin
  Result:='';
  if Size > 0 then
  begin
    Result:=IntToStr(Num[0]);
    for I := 1 to Size-1 do
    begin
      Result:=Result+'.'+IntToStr(Num[I]);
      if I >= 15 then Break
    end;
  end;
end;

procedure TVersion.SetString(S: string);
var
  I, V: UInt;
  Buf: String;
begin
  Buf:='';
  S:=S+'.';
  Errors:=0;
  Size:=0;
  if (S <> '.') then while Pos('.',S) <> 0 do
  begin
    Buf:=Copy(S, 1, Pos('.', S)-1);
    Delete(S, 1, Pos('.', S));
    for I:=Length(Buf) downto 1 do
      if Pos(Buf[I], SDec) = 0 then
        Delete(Buf, I, 1);
    if Buf = '' then
      Inc(Errors);
    V:=StrToIntDef(Buf,0);
    Num[Size]:=V;

    if Size=16 then
      Break;
  end
  else
    Inc(Errors);
end;

function TVersion.GetInt64: Int64;
begin
  Result:=MakeQword(Num[0], Num[1], Num[2], Num[3]);
end;

procedure TVersion.SetInt64(Val64: Int64);
begin
  Size:=4;
  Num[0]:=Word(Val64);
  Num[1]:=Word(Val64 shr 16);
  Num[2]:=Word(Val64 shr 32);
  Num[3]:=Word(Val64 shr 48);
end;

function TVersion.IsBetween(V1, V2: TVersion): Boolean;
var
  I: UInt;
  MaxSize: UInt;
begin
  Result:=false;
  MaxSize:=Size;
  if V1.Size > MaxSize then
    MaxSize:=V1.Size;
  if V2.Size > MaxSize then
    MaxSize:=V2.Size;

  for I := 0 to MaxSize do
  begin
    if Num[I] < V1[I] then
      Break
    else if Num[I] > V2[I] then
      Break
    else if (Num[I] > V1[I]) and (Num[I] < V2[I]) then
    begin
      Result:=true;
      Break
    end;
  end;
end;

function TVersion.IsBetween(S1, S2: String): Boolean;
var
  Left, Right: TVersion;
begin
  Left:=TVersion.Create(S1);
  Right:=TVersion.Create(S2);
  try
    Result:=IsBetween(Left, Right);
  finally
    Right.Free;
    Left.Free;
  end;
end;

function TVersion.IsOlderThan(V: TVersion): Boolean;
var
  I: UInt;
  MaxSize: UInt;
begin
  Result:=false;
  MaxSize:=Size;
  if V.Size > MaxSize then
    MaxSize:=V.Size;
    
  for I := 0 to MaxSize-1 do
  begin
    if Self.Num[I] < V.Num[I] then
    begin
      Result:=true;
      Break;
    end
    else if Self.Num[I] > V.Num[I] then
      Break
  end;
end;

function TVersion.IsOlderThan(S: String): Boolean;
var
  Target: TVersion;
begin
  Target:=TVersion.Create(S);
  try
    Result:=IsOlderThan(Target);
  finally
    Target.Free;
  end;
end;

function TVersion.IsAtLeast(V: TVersion): Boolean;
begin
  Result := not IsOlderThan(V);
end;

function TVersion.IsAtLeast(S: String): Boolean;
begin
  Result := not IsOlderThan(S);
end;

function TVersion.IsValid: Boolean;
begin
  Result:=false;
  if Errors = 0 then
    Result:=true;
end;

constructor TVersion.Create(S: string);
begin
  inherited Create;
  Str:=S;
end;

destructor TVersion.Destroy;
begin
  Size:=0;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}


function IsValidVerStr(Ver: String): Boolean;
begin
  with TVersion.Create(Ver) do
  try
    Result:=Valid;
  finally
    Free;
  end;
end;

function StrToVersion(Ver: String): TVersion;
begin
  Result:=TVersion.Create(Ver);
end;

function VersionNewerOrEquals(Check, Target: string): Boolean;
begin
  Result := not VersionOlder(Check, Target);
end;

function VersionOlder(Check, Target: string): Boolean;
begin
  with TVersion.Create(Check) do
  try
    Result:=IsOlderThan(Target);
  finally
    Free;
  end;
end;

function VersionBetween(Check, S1, S2: string): Boolean;
begin
  with TVersion.Create(Check) do
  try
    Result:=IsBetween(S1, S2);
  finally
    Free;
  end;
end;

function MakeLongLong(L1, L2: LongWord): Int64;
begin
  Result := Int64(L1) or Int64(L2) shl 32;
end;

function MakeQword(W1, W2, W3, W4: Word): Int64;
begin
  Result := MakeLongLong(W1 or W2 shl 16, W3 or W4 shl 16);
end;

end.

