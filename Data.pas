unit Data;

{-------------------------------------------------------------------------------
| Data encoding/compression/processing unit.
| (C) CyberShadow - 2006
| (C) StepS - 2013-2014
|-------------------------------------------------------------------------------
| FEATURES:
|
| - Encode and decode strings between various formats
| - Read and write text files into strings in multiple codepages with ease
| - Unicode (LE/BE), UTF-8 with Signature and ANSI are supported for text files
| - Parse language files (in the 'CODE "string"' format)
| - Dissect strings to more easily get segments separated with whitespaces
| - Generate random alphanumeric strings
| - Get W:A URLs
| - Acknowledges OS/Compiler differences: UTF-8 output on Linux, U/A on Windows
-------------------------------------------------------------------------------}

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface

uses
  SysUtils;

type
{$IF Defined(MSWINDOWS) and (CompilerVersion >= 20)}
  CyrillicString = type AnsiString(1251);
{$ELSEIF Defined(MSWINDOWS)}
  UnicodeString = WideString;
  CyrillicString = AnsiString;
{$ELSE}
  CyrillicString = AnsiString;
{$IFEND}

const
  CP_ANSI = 0;
  CP_UNICODE = 1;
  CP_UNICODE_BE = 2;
  CP_UTF8_SIGNED = 3;
  CP_UTF8 = 4;

  SBin = '01';
  SOct = SBin+'234567';
  SDec = SOct+'89';
  SHex = SDec+'ABCDEF';
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  Codes64 = SDec+Alphabet+'+/';

function StrEncode64(S: string): string;
function StrDecode64(S: string): string;

function StrEncodePURLE(S: string): string;
function StrDecodePURLE(S: string): string;
                              
function LowerFCStr(S: string): string;
function UpperFCStr(S: string): string;
function ULPos(Substr, Str: string): Integer;

function DetermineCodepage(ByteMask: string): ShortInt;
function CP_WAto1251(SX: string): string;
function SetToStr(first, last: char): string;

function TextMatch(S1, S2: string): Boolean;

function RandomString(Length: LongWord): string;
function ContinuedSection(S: string; N: integer): string;
function StringSection(S: string; N: integer): string;
function GetTrail(S: string): string;
procedure DeleteSections(var S: string; Amount: integer);
procedure CutLeadingColon(var S: string);

function GetLocIDFromString(S: string): string;
function GetLocValueFromString(S: string): string;
function SeqParseStr(S: string; Wide: Boolean = false): string;
function MakeWALink(Address, ID, Scheme: string): string;

function GetLine(var Source, Dest: string): Boolean;
function StrToHex(S: string): string;
function GetTextFile(FN: string): string;
function GetBinaryFileAsString(FN: string): string;
function TextToFile(S, FN: string; ForceCreate: Boolean=false): Word;
function ForceDirectoriesEx(Path: String): Boolean;

implementation

function StrEncode64(S: string): string;
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

function StrDecode64(S: string): string;
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

procedure ConvertPURLEChar(var S: string; C: char);
begin
  if Pos('%',S) <> 0 then
    S:=Copy(S, 1, Pos('%',S)-1) + C + Copy(S, Pos('%',S)+3, Length(S) - Pos('%',S)+2);
end;

function StrEncodePURLE(S: string): string;
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

function StrDecodePURLE(S: string): string;
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
        for J:=1 to Length(SHex) do
          if SChar[I] = SHex[J] then
          begin
            Valid[I]:=true;
            Break
          end;
      if Valid[1] and Valid[2] then
      begin
        RChar:=Chr(StrToInt('$'+SChar));
        if (RChar <> '%') and (RChar <> '<') and (RChar <> '>') and (Ord(RChar) > 32) then
          ConvertPURLEChar(S,RChar)
        else
        case RChar of
        '%': ConvertPURLEChar(S,#30);
        '<': ConvertPURLEChar(S,#19);
        '>': ConvertPURLEChar(S,#20);
        #32: ConvertPURLEChar(S,#160);
        #21: ConvertPURLEChar(S,#92);
        #18: ConvertPURLEChar(S,#39);
        #17: ConvertPURLEChar(S,#38);
        #16: ConvertPURLEChar(S,#37);
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

function DetermineCodepage(ByteMask: string): ShortInt;
begin
  Result := CP_ANSI;
  if Copy(ByteMask, 1, 2) = #$FF#$FE then Result := CP_UNICODE
  else if Copy(ByteMask, 1, 2) = #$FE#$FF then  Result := CP_UNICODE_BE
  else if Copy(ByteMask, 1, 3) = #$EF#$BB#$BF then  Result := CP_UTF8_SIGNED;
end;

function CP_WAto1251(SX: string): string;
var
  I: Integer;
  S: CyrillicString;
begin
  S:=CyrillicString(SX);
  for I:=1 to Length(S) do
    if S[I] in [#$80..#$FF] then
      case S[I] of
        #$80: S[I]:=#$C1;   #$90: S[I]:=#$DA;                       #$B0: S[I]:=#$FB;
        #$81: S[I]:=#$C3;   #$91: S[I]:=#$DB;   #$A1: S[I]:=#$21;   #$B1: S[I]:=#$FC;
        #$82: S[I]:=#$C4;   #$92: S[I]:=#$DC;   #$A2: S[I]:=#$EA;   #$B2: S[I]:=#$FD;
        #$83: S[I]:=#$C6;   #$93: S[I]:=#$DD;   #$A3: S[I]:=#$50;   #$B3: S[I]:=#$FE;
        #$84: S[I]:=#$C7;   #$94: S[I]:=#$DE;                       #$B4: S[I]:=#$FF;
        #$85: S[I]:=#$C8;                       #$A5: S[I]:=#$EB;   #$B5: S[I]:=#$4F;
        #$86: S[I]:=#$C9;   #$96: S[I]:=#$DF;   #$A6: S[I]:=#$EC;   #$B6: S[I]:=#$6F;
        #$87: S[I]:=#$CA;   #$97: S[I]:=#$E1;   #$A7: S[I]:=#$ED;   #$B7: S[I]:=#$55;
        #$88: S[I]:=#$CB;   #$98: S[I]:=#$E2;   #$A8: S[I]:=#$EF;   #$B8: S[I]:=#$75;
        #$89: S[I]:=#$CF;   #$99: S[I]:=#$E3;   #$A9: S[I]:=#$F2;
        #$8A: S[I]:=#$D3;   #$9A: S[I]:=#$E4;   #$AA: S[I]:=#$F4;
        #$8B: S[I]:=#$D4;   #$9B: S[I]:=#$E6;   #$AB: S[I]:=#$F6;
        #$8C: S[I]:=#$D6;   #$9C: S[I]:=#$E7;   #$AC: S[I]:=#$F7;
        #$8D: S[I]:=#$D7;   #$9D: S[I]:=#$E8;   #$AD: S[I]:=#$F8;
        #$8E: S[I]:=#$D8;   #$9E: S[I]:=#$E9;   #$AE: S[I]:=#$F9;
        #$8F: S[I]:=#$D9;   #$9F: S[I]:=#$59;   #$AF: S[I]:=#$FA;   #$BF: S[I]:=#$3F;

        #$CB: S[I]:=#$A8;
        #$EB: S[I]:=#$B8;
      end;

  Result:=String(S);
end;

{$IF not defined(LINUX) and (CompilerVersion < 20)}
function UTF8ToString(const s: UTF8String): UnicodeString;
begin
  Result := UTF8Decode(s);
end;
{$IFEND}

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

function GetLocIDFromString(S: string): string;
begin
  if not GetLine(S, Result) then
    Result:=S;

  Result:=Trim(Copy(S, 1, Pos('"', S+'"')-1));
  if Pos(' ', Result) <> 0 then
    Result:=Copy(Result, 1, Pos(' ', Result)-1);
  if Pos(#9, Result) <> 0 then
    Result:=Copy(Result, 1, Pos(#9, Result)-1);
  if Pos(',', Result) = Length(Result) then
    Delete(Result, Length(Result), 1);
end;

function GetLocValueFromString(S: string): string;
var
  Wide: Boolean;
begin
  Result:='';
  if not GetLine(S, Result) then
    Result:=S;

  Wide:=false;
  if Pos('L"', Result) = Pos('"', Result)-1 then
    Wide:=true;
  Delete(Result, 1, Pos('"', Result+'"'));
  Result:=SeqParseStr(Result, Wide);
end;

function SeqParseStr(S: string; Wide: boolean): string;
var
  I: Integer;
begin
  Result:='';
  I:=1;
  if Pos('"', S) = 1 then Inc(I);
  while I <= Length(S) do
  begin
    case S[I] of
    '\':
        if I < Length(S) then
        begin
          case S[I+1] of
          'a': Result:=Result+#7;
          'b': Result:=Result+#8;
          't': Result:=Result+#9;
          'n': Result:=Result+#10;
          'v': Result:=Result+#11;
          'f': Result:=Result+#12;
          'r': Result:=Result+#13;
          'x': begin
               Result:=Result+Chr(StrToIntDef('$'+Copy(S, I+2, Byte(Wide)*2), Ord('?')));
               Inc(I, Byte(Wide)*2);
               end;
          else Result:=Result+S[I+1];
          end;
          Inc(I);
        end;
    '"':
        Break;
    else
        Result:=Result+S[I];
    end;
    Inc(I);
  end;
end;

function MakeWALink(Address, ID, Scheme: string): string;
begin
  Result:=Format('wa://%s?ID=%s&Scheme=%s', [Address, ID, Scheme]);
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
    Result:=Result+SHex[Ord(S[I]) shr 4]+SHex[Ord(S[I]) and $F]+' ';
  Result:=Result+'| ';
  for I:=1 to Length(S) do
  if AnsiChar(S[I]) in [#32..#126] then
    Result:=Result+S[I]
   else
    Result:=Result+'.';
end;

function RandomString(Length: LongWord): string;
var
  I: Integer;
begin
  Result:='';
  Randomize;
  for I := 1 to Length do
  begin
    case Random(3) of
      0:
        Result:=Result+Chr(Ord('0') + Random(10));
      1:
        Result:=Result+Chr(Ord('A') + Random(26));
      2:
        Result:=Result+Chr(Ord('a') + Random(26));
    end;
  end;
end;

function ContinuedSection(S: string; N: integer): string;
begin
  DeleteSections(S, N);
  Result:=S;
end;

function StringSection(S: string; N: integer): string;
begin
  DeleteSections(S, N);
  Result:=Copy(S,1,Pos(' ',S+' ')-1);
end;

function GetTrail(S: string): string;
begin
  Result:='';
  if Pos(' :', S) <> 0 then
  begin
    Delete(S, 1, Pos(' :', S)+1);
    Result:=S;
  end;
end;

procedure DeleteSections(var S: string; Amount: integer);
var I: Integer;
begin
  for I := 1 to Amount do
    Delete(S,1,Pos(' ',S+' '));
end;

procedure CutLeadingColon(var S: string);
begin
  if Pos(':',S) = 1 then
    Delete(S, 1, 1);
end;

function GetTextFile(FN: string): string;  //get text file in ANSI, Unicode and signed UTF-8
var
  F: File;
  I, ReqMax, CP: Integer;
  AStr: AnsiString;
  UStr: UTF8String;
  WStr: UnicodeString;
begin
  Result:='';
  {$I-}
  if FileExists(FN) then
  begin
    AssignFile(F, FN);
    Reset(F, 1);
    if FileSize(F) > 0 then
    begin
      ReqMax:=4;
      if FileSize(F) < ReqMax then
        ReqMax:=FileSize(F);
      SetLength(AStr, ReqMax);
      BlockRead(F, AStr[1], ReqMax);
      CP:=DetermineCodepage(String(AStr));
      case CP of
      CP_UNICODE..CP_UNICODE_BE:
        if FileSize(F) > 2 then
        begin
          Reset(F, 2);
          Seek(F, 1);
          SetLength(WStr, FileSize(F)-1);
          BlockRead(F, WStr[1], FileSize(F)-1);
          if CP = CP_UNICODE_BE then for I := 1 to Length(WStr) do WStr[I]:=WideChar(Swap(Word(WStr[I])));
          {$IFDEF LINUX}
          Result:=UTF8Encode(WStr);
          {$ELSE}
          Result:=String(WStr);
          {$ENDIF}
        end;
      CP_UTF8_SIGNED:
        if FileSize(F) > 3 then
        begin
          Seek(F, 3);
          SetLength(UStr, FileSize(F)-3);
          BlockRead(F, UStr[1], FileSize(F)-3);
          {$IFDEF LINUX}
          Result:=String(UStr);
          {$ELSE}
          Result:=String(UTF8ToString(UStr));
          {$ENDIF}
        end;
      else
        begin
          Seek(F, 0);
          SetLength(AStr, FileSize(F));
          BlockRead(F, AStr[1], FileSize(F));
          {$IFDEF LINUX}
          Result:=String(UTF8Encode(AStr));
          {$ELSE}
          Result:=String(AStr);
          {$ENDIF}
        end;
      end;
    end;
    Result:=Result+#13#10;
    CloseFile(F);
  end;
  {$I+}
end;

function GetBinaryFileAsString(FN: string): string;
var
  F: File;
  AStr: AnsiString;
begin
  Result:='';
  {$I-}
  if FileExists(FN) then
  begin
    AssignFile(F, FN);
    Reset(F, 1);
    SetLength(AStr, FileSize(F));
    BlockRead(F, AStr[1], FileSize(F));
    Result:=String(AStr);
    CloseFile(F);
  end;
  {$I+}
end;

function TextToFile(S, FN: string; ForceCreate: Boolean=false): Word;
var
  F: text;
begin
  {$I-}
  AssignFile(F, FN);
  if ForceCreate and not FileExists(FN) then
  begin
    ForceDirectoriesEx(FN);
    Rewrite(F);
  end;
  Append(F);
  WriteLn(F, S);
  CloseFile(F);
  {$I+}
  Result:=IOResult;
end;

function ForceDirectoriesEx(Path: String): Boolean;
begin
  {$I-}
  if Pos(PathDelim, Path) <> 0 then
    Result:=ForceDirectories(Copy(Path, 1, LastDelimiter(PathDelim, Path)-1))
  else
    Result:=false;
  {$I+}
end;

end.

