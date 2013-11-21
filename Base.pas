unit Base;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

const
  APPVERSION = '1.3.1.1';
  
var
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort, VerboseLogging: Integer;
  IRCOperPassword: string;
  IRCChannel: string;
  StealthIP: string;
  NetworkName: string;
  StartupTime: string;

  IPBans, NickBans: array of String;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
procedure EventLog(S: string; DiskOnly: Boolean=False);
function WinSockErrorCodeStr(Code: Integer): string;

procedure LoadBanlists;

function BannedIP(IP: String): Boolean;
function BannedNick(Nick: String): Boolean;

function IRCDateTimeNow : Int64;
function TextDateTimeNow : string;

implementation
uses
{$IFNDEF WIN32}
  UnixUtils,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, IRCServer;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
var
  F: text;
begin
  if Copy(S, 1, 1)<>'-' then
    S:='['+TimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file WNServer.log exists
  {$I-}
  Assign(F, ExtractFilePath(ParamStr(0))+'WNServer.log');
  Append(F);
  WriteLn(F, S);
  Close(f);
  {$I+}
  if IOResult<>0 then ;

  if (VerboseLogging > 0) or (Important) then
    if not DiskOnly then
    begin
      // logging to console, if it's enabled
      {$I-}
      WriteLn(S);
      {$I+}
      if IOResult<>0 then ;

      // echo to IRC OPERs
      LogToOper(S);
    end;
end;

procedure EventLog(S: string; DiskOnly: Boolean=false);
var
  F: text;
begin
  Log(S,DiskOnly,true);

  if Copy(S, 1, 1)<>'-' then
    S:='['+DateTimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file EventLog.log exists
  {$I-}
  Assign(F, ExtractFilePath(ParamStr(0))+'EventLog.log');
  Append(F);
  WriteLn(F, S);
  Close(f);
  {$I+}
  if IOResult<>0 then ;
end;

procedure LoadBanlists;
var
  F: text;
  FilePath: String;
begin
  FilePath:=ExtractFilePath(ParamStr(0))+'banlist_nicks.txt';
  Assign(F,FilePath);
  if not FileExists(FilePath) then
  begin
    Rewrite(F);
    WriteLn(F, 'fuck');
    SetLength(NickBans,1);
    NickBans[0]:='fuck';
    Close(F);
  end
  else
  begin
    Reset(F);
    while not EOF(F) do
    begin
      SetLength(NickBans, Length(NickBans)+1);
      ReadLn(f, NickBans[Length(NickBans)-1]);
    end;
    Close(F);
  end;

  FilePath:=ExtractFilePath(ParamStr(0))+'banlist_ips.txt';
  Assign(F,FilePath);
  if not FileExists(FilePath) then
  begin
    Rewrite(F);
    WriteLn(F, '0.0.0.0');
    SetLength(IPBans,1);
    IPBans[0]:='0.0.0.0';
    Close(F);
  end
  else
  begin
    Reset(F);
    while not EOF(F) do
    begin
      SetLength(IPBans, Length(IPBans)+1);
      ReadLn(f, IPBans[Length(IPBans)-1]);
    end;
    Close(F);
  end;
end;

function BannedIP(IP: String): Boolean;
var I: Integer;
begin
  Result:=false;
  for I:=0 to Length(IPBans)-1 do
  begin
    if IPBans[I] = IP then
    begin
      Result := true;
      Break;
    end;
  end;
end;

function BannedNick(Nick: String): Boolean;
var I: Integer;
begin
  Result:=false;
  for I:=0 to Length(NickBans)-1 do
    if UpperCase(NickBans[I]) = UpperCase(Nick) then
    begin
      Result := true;
      Break;
    end;
end;

{$IFDEF WIN32}

{$INCLUDE WinSockCodes.inc}

function WinSockErrorCodeStr(Code: Integer): string;
var
  I: Integer;
begin
  Result:='Error #'+IntToStr(Code);
  for I:=1 to High(WinSockErrors) do
    if (WinSockErrors[I].Code=Code)or(WinSockErrors[I].Code=Code+10000) then
      Result:=WinSockErrors[I].Text;
end;

{$ELSE}

function WinSockErrorCodeStr(Code: Integer): string;
begin
  Result:=StrError(Code);
end;

{$ENDIF}

{$IFDEF WIN32}

function IRCDateTimeNow : Int64;
var
  Timezone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(Timezone);
  Result := Round(Now * SecsPerDay) + (Timezone.Bias * 60) - 2209176000;
end;

function TextDateTimeNow : string;
var
  Timezone: TTimeZoneInformation;
  Side: Char;
  UTCOffset: TDateTime;
  StrOffset: String;
begin
  GetTimeZoneInformation(Timezone);
  UTCOffset := Timezone.Bias/MinsPerDay;
  StrOffset := TimeToStr(UTCOffset);
  StrOffset := Copy(StrOffset,1,Length(StrOffset)-3);
  if UTCOffset>0 then Side:='-'
  else Side:='+';
  Result := DateTimeToStr(Now)+' UTC'+Side+StrOffset;
end;

{$ELSE}

function IRCDateTimeNow : Int64;
begin
  Result := Round(Now * SecsPerDay) - 2209176000;
end;

function TextDateTimeNow : string;
begin
  Result := DateTimeToStr(Now)+' server local time';
end;

{$ENDIF}

end.
