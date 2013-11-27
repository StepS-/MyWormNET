unit Data;  // data encoding/compression/processing

{$I cDefines.inc}

interface

function Encode64(S: string): string;
function Decode64(S: string): string;
                              
function LowerFCStr(S: string): string;
function UpperFCStr(S: string): string;

function GetLine(var Source, Dest: string): Boolean;
function StrToHex(S: string): string;
function GetFile(FN: string): string;

implementation

uses
  SysUtils;

const
  Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';

function Encode64(S: string): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;

function Decode64(S: string): string;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + chr(x);
      end;
    end
    else
      Exit;
  end;
end;

function LowerFCStr(S: string): string;
begin
  Result:=LowerCase(S[1])+Copy(S,2,Length(S));
end;

function UpperFCStr(S: string): string;
begin
  Result:=UpperCase(S[1])+Copy(S,2,Length(S));
end;

function GetLine(var Source, Dest: string): Boolean;
var
  P, P1, P2: Integer;
begin
  P1:=Pos(#13, Source);
  P2:=Pos(#10, Source);
  if (P1=0) and (P2=0) then
  begin
    Dest:='';
    Result:=False;
    Exit
  end
  else
    if P1=0 then
      P:=P2
    else
    if P2=0 then
      P:=P1
    else
    if(P1<P2) then
      P:=P1
    else
      P:=P2;
  Result:=True;
  Dest:=Copy(Source, 1, P-1);
  Delete(Source, 1, P);
  if Copy(Source, 1, 1)=#10 then
    Delete(Source, 1, 1);
end;

function StrToHex(S: string): string;
const
  Hex='0123456789ABCDEF';
var
  I: Integer;
begin
  Result:='';
  for I:=1 to Length(S) do
    Result:=Result+Hex[Ord(S[I]) shr 4]+Hex[Ord(S[I]) and $F]+' ';
  Result:=Result+'| ';
  for I:=1 to Length(S) do
  {$IFDEF DELPHI2009_UP}
  if CharInSet(S[I], [#32..#126]) then
  {$ELSE}
  if S[I] in [#32..#126] then
  {$ENDIF}
    Result:=Result+S[I]
   else
    Result:=Result+'.';
end;

function GetFile(FN: string): string;
var
  F: File;
  AStr: AnsiString;
begin
  if FileExists(FN) then
  begin
    Assign(F, FN);
    Reset(F, 1);
    SetLength(AStr, FileSize(F));
    BlockRead(f, Astr[1], FileSize(F));
    Result:=String(AStr);
    CloseFile(F);
  end
  else
    Result:='';
end;


end.
