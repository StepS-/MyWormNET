unit Data;  // data encoding/compression/processing
{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

type
  TIntArray = array of Integer;

function Encode64(S: string): string;
function Decode64(S: string): string;

function EncodeURI(S: string): string;
function DecodeURI(S: string): string;
                              
function LowerFCStr(S: string): string;
function UpperFCStr(S: string): string;
function ULPos(Substr, Str: string): Integer;

function CP_WAto1251(SX: string): string;
function SetToStr(first, last: char): string;

function TextMatch(S1, S2: string): Boolean;

function VersionThisNewer(Check, Target: string): Boolean;
function VersionOlder(Check, Target: string): Boolean;
function VersionBetween(Check, V1, V2: string): Boolean;

function GetLine(var Source, Dest: string): Boolean;
function StrToHex(S: string): string;
function GetFile(FN: string): string;
function TextToFile(S, FN: string; ForceCreate: Boolean=false): Word;

procedure EqualizeIntArrays(var Array1, Array2: TIntArray);
procedure VerStrToArray(Ver: String; var OutArray: TIntArray);

implementation

uses
  SysUtils;

const
  Codes64 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
  Hex = '0123456789ABCDEF';

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

procedure ConvertURIChar(var S: string; C: char);
begin
  if Pos('%',S) <> 0 then
    S:=Copy(S, 1, Pos('%',S)-1) + C + Copy(S, Pos('%',S)+3, Length(S) - Pos('%',S)+2);
end;

function EncodeURI(S: string): string;
const
  BadChars='''<>!$()[]*+,;\| ';
var
  I: Integer;
begin
  for I:=1 to Length(BadChars) do
    while Pos(BadChars[I], S) <> 0 do
      S:=Copy(S, 1, Pos(BadChars[I],S)-1) + '%' + IntToHex(Ord(BadChars[I]),2) + Copy(S, Pos(BadChars[I],S)+1, Length(S) - Pos(BadChars[I],S));
  Result:=S;
end;

function DecodeURI(S: string): string;
var
  I, J: Integer;
  Valid: array[1..2] of Boolean;
  SChar: string;
  RChar: Char;
begin
  while Pos('%',S) <> 0 do
  begin
    if Pos('%',S)+2 <= Length(S) then
    begin
      SChar:=UpperCase(Copy(S, Pos('%',S)+1, 2));
      for I:=1 to 2 do
        for J:=1 to Length(Hex) do
          if SChar[I] = Hex[J] then
          begin
            Valid[I]:=true;
            Break
          end;
      if Valid[1] and Valid[2] then
      begin
        RChar:=Chr(StrToInt('$'+SChar));
        if (RChar <> '%') and (RChar <> '<') and (RChar <> '>') and (Ord(RChar) > 32) then
            ConvertURIChar(S,RChar)
        else
        case RChar of
        '%': ConvertURIChar(S,#30);
        '<': ConvertURIChar(S,#19);
        '>': ConvertURIChar(S,#20);
        #32: ConvertURIChar(S,#160);
        #21: ConvertURIChar(S,#92);
        #18: ConvertURIChar(S,#39);
        #17: ConvertURIChar(S,#38);
        #16: ConvertURIChar(S,#37);
        else
          Delete(S, Pos('%',S), 3);
        end;
      end
      else
        S[Pos('%',S)]:=#30;
    end
    else
      S[Pos('%',S)]:=#30;
  end;
  if Pos(#30,S) <> 0 then
    for I := 1 to Length(S) do
      if S[I] = #30 then
        S[I] := '%';
  Result:=S;
end;

function LowerFCStr(S: string): string;
begin
  Result:=LowerCase(S[1])+Copy(S,2,Length(S)-1);
end;

function UpperFCStr(S: string): string;
begin
  Result:=UpperCase(S[1])+Copy(S,2,Length(S)-1);
end;

function ULPos(Substr, Str: string): Integer;
begin
  Result:=Pos(UpperCase(Substr),UpperCase(Str));
end;

function CP_WAto1251(SX: string): string;
var
  I: Integer;
  S: AnsiString;
begin
  S:=AnsiString(SX);
  for I:=1 to Length(S) do
    if S[I] in [#$80..#$BF] then
      case S[I] of
        #$80: S[I]:=#$C1;   #$90: S[I]:=#$DA;   #$A0: S[I]:=#$A0;   #$B0: S[I]:=#$FB;
        #$81: S[I]:=#$C3;   #$91: S[I]:=#$DB;   #$A1: S[I]:=#$21;   #$B1: S[I]:=#$FC;
        #$82: S[I]:=#$C4;   #$92: S[I]:=#$DC;   #$A2: S[I]:=#$EA;   #$B2: S[I]:=#$FD;
        #$83: S[I]:=#$C6;   #$93: S[I]:=#$DD;   #$A3: S[I]:=#$50;   #$B3: S[I]:=#$FE;
        #$84: S[I]:=#$C7;   #$94: S[I]:=#$DE;   #$A4: S[I]:=#$A4;   #$B4: S[I]:=#$FF;
        #$85: S[I]:=#$C8;   #$95: S[I]:=#$95;   #$A5: S[I]:=#$EB;   #$B5: S[I]:=#$4F;
        #$86: S[I]:=#$C9;   #$96: S[I]:=#$DF;   #$A6: S[I]:=#$EC;   #$B6: S[I]:=#$6F;
        #$87: S[I]:=#$CA;   #$97: S[I]:=#$E1;   #$A7: S[I]:=#$ED;   #$B7: S[I]:=#$55;
        #$88: S[I]:=#$CB;   #$98: S[I]:=#$E2;   #$A8: S[I]:=#$EF;   #$B8: S[I]:=#$75;
        #$89: S[I]:=#$CF;   #$99: S[I]:=#$E3;   #$A9: S[I]:=#$F2;   #$B9: S[I]:=#$B9;
        #$8A: S[I]:=#$D3;   #$9A: S[I]:=#$E4;   #$AA: S[I]:=#$F4;   #$BA: S[I]:=#$BA;
        #$8B: S[I]:=#$D4;   #$9B: S[I]:=#$E6;   #$AB: S[I]:=#$F6;   #$BB: S[I]:=#$BB;
        #$8C: S[I]:=#$D6;   #$9C: S[I]:=#$E7;   #$AC: S[I]:=#$F7;   #$BC: S[I]:=#$BC;
        #$8D: S[I]:=#$D7;   #$9D: S[I]:=#$E8;   #$AD: S[I]:=#$F8;   #$BD: S[I]:=#$BD;
        #$8E: S[I]:=#$D8;   #$9E: S[I]:=#$E9;   #$AE: S[I]:=#$F9;   #$BE: S[I]:=#$BE;
        #$8F: S[I]:=#$D9;   #$9F: S[I]:=#$59;   #$AF: S[I]:=#$FA;   #$BF: S[I]:=#$3F;
      end;
  Result:=String(S);
end;

function SetToStr(first, last: char): string;
var I: Integer;
begin
  Result:='';
  if ord(first) <= ord(last) then
    for I := ord(first) to ord(last) do
      Result := Result + Chr(I);
end;

function TextMatch(S1, S2: string): Boolean;
begin
  Result:=false;
  if UpperCase(S1) = UpperCase(S2) then
    Result:=true;
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
var
  I: Integer;
begin
  Result:='';
  for I:=1 to Length(S) do
    Result:=Result+Hex[Ord(S[I]) shr 4]+Hex[Ord(S[I]) and $F]+' ';
  Result:=Result+'| ';
  for I:=1 to Length(S) do
  if AnsiChar(S[I]) in [#32..#126] then
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
    BlockRead(F, AStr[1], FileSize(F));
    Result:=String(AStr);
    CloseFile(F);
  end
  else
    Result:='';
end;

function TextToFile(S, FN: string; ForceCreate: Boolean=false): Word;
var
  F: text;
begin
  {$I-}
  Assign(F, FN);
  if ForceCreate then
    if not FileExists(FN) then
      Rewrite(F);
  Append(F);
  WriteLn(F, S);
  Close(F);
  {$I+}
  Result:=IOResult;
end;

procedure EqualizeIntArrays(var Array1, Array2: TIntArray);
var
  I: Integer;
begin
  if Length(Array1) > Length(Array2) then
    for I := Length(Array2)+1 to Length(Array1) do
    begin
      SetLength(Array2,I);
      Array2[I-1]:=0;
    end
  else if Length(Array1) < Length(Array2) then
    for I := Length(Array1)+1 to Length(Array2) do
    begin
      SetLength(Array1,I);
      Array1[I-1]:=0;
    end;
end;

procedure VerStrToArray(Ver: String; var OutArray: TIntArray);
var
  V: Integer;
  Buf: String;
begin
  Ver:=Ver+'.';
  while Pos('.',Ver) <> 0 do
  begin
    Buf:=Copy(Ver, 1, Pos('.', Ver)-1);
    Delete(Ver, 1, Pos('.', Ver));
    V:=StrToIntDef(Buf,-1);
    SetLength(OutArray,Length(OutArray)+1);
    if V >= 0 then
      OutArray[Length(OutArray)-1]:=V
    else
      OutArray[Length(OutArray)-1]:=0;
    if Length(OutArray)=6 then
      Break;
  end;
end;

function VersionThisNewer(Check, Target: string): Boolean;
var
  I: Integer;
  CheckNum, TargetNum: TIntArray;
begin
  Result:=false;
  if (Check <> '') and (Target <> '') then
  begin
    VerStrToArray(Check, CheckNum);
    VerStrToArray(Target, TargetNum);
    EqualizeIntArrays(CheckNum, TargetNum);
    for I := 0 to Length(CheckNum)-1 do
    begin
      if CheckNum[I] > TargetNum[I] then
      begin
        Result:=true;
        Break;
      end
      else
        if CheckNum[I] = TargetNum[I] then
          if I=Length(CheckNum)-1 then
          begin
            Result:=true;
            Break;
          end
          else
            continue
        else
          Break;
    end;
  end;
end;

function VersionOlder(Check, Target: string): Boolean;
var
  I: Integer;
  CheckNum, TargetNum: TIntArray;
begin
  Result:=false;
  if (Check <> '') and (Target <> '') then
  begin
    VerStrToArray(Check, CheckNum);
    VerStrToArray(Target, TargetNum);
    EqualizeIntArrays(CheckNum, TargetNum);
    for I := 0 to Length(CheckNum)-1 do
    begin
      if CheckNum[I] < TargetNum[I] then
      begin
        Result:=true;
        Break;
      end
      else
        if CheckNum[I] > TargetNum[I] then
          Break
        else
          continue;
    end;
  end;
end;

function VersionBetween(Check, V1, V2: string): Boolean;
begin
  Result:=false;
end;

end.
