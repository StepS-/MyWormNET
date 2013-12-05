unit Base;

{$I cDefines.inc}

{$IFDEF FPC}
{$modeswitch DELPHI}
{$ENDIF}

interface
const
  APPVERSION = '1.3.5.0';

var
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort: Integer;
  IRCOperPassword: string;
  IRCChannel: string;
  StealthIP: string;
  NetworkName: string;
  StartupTime: string;
  VerboseLogging: string;

  IPBans, NickBans: array of String;

{$I locale.inc}

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
procedure EventLog(S: string; DiskOnly: Boolean=False);
function WinSockErrorCodeStr(Code: Integer): string;

procedure LoadBanlists;
procedure LoadParams;

function BannedIP(IP: string): Boolean;
function BannedNick(Nick: string): Boolean;

function IRCDateTimeNow : Int64;
function TextDateTimeNow : string;

implementation
uses
{$IFDEF OS_MSWIN}
  Windows,
{$ELSE}
  errors,
{$ENDIF}
  IniFiles, SysUtils, IRCServer, Data;

procedure LoadParams;
var
  Config: TMemIniFile;
begin
  Config := TMemIniFile.Create(ExtractFilePath(ParamStr(0))+'WNServer.ini');
  ServerHost      :=Config.ReadString ('WormNet','ServerHost',     'localhost');
  IRCPort         :=Config.ReadInteger('WormNet','IRCPort',               6667);
  HTTPPort        :=Config.ReadInteger('WormNet','HTTPPort',                80);
  WormNATPort     :=Config.ReadInteger('WormNet','WormNATPort',          17018);
  VerboseLogging  :=Config.ReadString ('WormNet','VerboseLogging',      'false');
  IRCOperPassword :=Config.ReadString ('WormNet','IRCOperPassword', 'password');
  StealthIP       :=Config.ReadString ('WormNet','StealthIP',      'no.address.for.you');
  NetworkName     :=Config.ReadString ('WormNet','NetworkName',      'MyWormNET');

  ServerHost:=Copy(ServerHost,1,Pos(' ',ServerHost+' ')-1);
  IRCOperPassword:=Copy(IRCOperPassword,1,Pos(' ',IRCOperPassword+' ')-1);
  while Pos(' ',StealthIP) <> 0 do
    StealthIP[Pos(' ',StealthIP)]:=#160;
  while Pos(' ',NetworkName) <> 0 do
    NetworkName[Pos(' ',NetworkName)]:=#160;

end;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
var
  F: text;
begin
//  if Copy(S, 1, 1)<>'-' then
  S:='['+TimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file WNServer.log exists
  TextToFile(S, ExtractFilePath(ParamStr(0))+'WNServer.log');

  if (VerboseLogging = 'true') or (Important) then
    if not DiskOnly then
    begin
      // logging to console, if it's enabled
      {$I-}
      WriteLn(S);
      {$I+}
      if IOResult<>0 then ;
    end;
end;

procedure EventLog(S: string; DiskOnly: Boolean=false);
begin
  Log(S,DiskOnly,true);

  if Copy(S, 1, 1)<>'-' then
    S:='['+DateTimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file EventLog.log exists
  TextToFile(S, ExtractFilePath(ParamStr(0))+'EventLog.log');
  // echo to IRC OPERs
  LogToOper(S);
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
    WriteLn(F, 'HostingBuddy');
    SetLength(NickBans,1);
    NickBans[0]:='HostingBuddy';
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
    WriteLn(F, '1.1.1.1');
    SetLength(IPBans,1);
    IPBans[0]:='1.1.1.1';
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

{$IFDEF OS_MSWIN}

{$INCLUDE WinSockCodes.inc}

function WinSockErrorCodeStr(Code: Integer): string;
var
  I: Integer;
begin
  Result:=L_ERROR+' #'+IntToStr(Code);
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

function IRCDateTimeNow : Int64;
begin
  Result := Round(Now * SecsPerDay) - 2209176000;
end;

{$IFDEF OS_MSWIN}

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

function TextDateTimeNow : string;
begin
  Result := DateTimeToStr(Now)+' server local time';
end;

{$ENDIF}

end.
