unit Base;

{-------------------------------------------------------------------------------
| Base unit for logging and general settings.
| (C) CyberShadow - 2006
| (C) StepS - 2013-2014
|-------------------------------------------------------------------------------
| FEATURES:
|
| - Load and reload the server settings
| - Log into text files WNServer.log (verbose) and EventLog.log (important)
| - Some timestamp-related extensions: OS/Compiler differences acknowledged
-------------------------------------------------------------------------------}

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface
uses
  IniFiles, DateUtils, Version, Lists;

const
  APPVERSION = '1.3.7.0';

var
  Config: TMemIniFile;
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort: Word;
  StartupTime, CreationTime: string;
  IRCOperPassword: string;
  IRCPassword: string;
  StealthIP: string;
  NetworkName: string;
  AppLanguage: string;
  MinimumVersion: TVersion;
  VerboseLogging, AllowArbitrary, SeenService, HTTPtoConsole: Boolean;
  ChatAntiFlood: Boolean;

  AntiFloodFactor: Double=1.0;
  MaxIRCUsers: Integer=0;
  IRCConnections: Int64=0;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
procedure EventLog(S: string; DiskOnly: Boolean=False);
function WinSockErrorCodeStr(Code: Integer): string;

procedure LoadParams;

function TextDateTime(T: TDateTime; IsUTC: Boolean = false) : string;
function ExecutableTimestamp: TDateTime;

implementation
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  errors,
{$ENDIF}
  SysUtils, SyncObjs,
  HTTPServer, IRCServer, WormNATServer, Localization, Data;

var
  PCS, LCS, ECS: TCriticalSection;

procedure InitSettings;
begin
  if Config = nil then
  begin
    ChDir(ExtractFilePath(ExpandFileName(ParamStr(0))));
    StartupTime   :=TextDateTime(Now);
    CreationTime  :=TextDateTime(ExecutableTimestamp, true);
    EventLog('------------------ '+DateTimeToStr(Now)+' ------------------',true);
    Config := TMemIniFile.Create('WNServer.ini');
    AppLanguage   :=Config.ReadString ('WormNet','Language',        'Default');
    GetLocalizations(AppLanguage);
    EventLog(Format(L_START, [APPVERSION]));
    IRCPort       :=Config.ReadInteger('WormNet','IRCPort',         6667);
    HTTPPort      :=Config.ReadInteger('WormNet','HTTPPort',        80);
    WormNATPort   :=Config.ReadInteger('WormNet','WormNATPort',     0);
  end;
end;

procedure LoadParams;
begin
  PCS.Enter;
  AppLanguage         :=Config.ReadString ('WormNet','Language',        'Default');
  ServerHost          :=Config.ReadString ('WormNet','ServerHost',      'localhost');
  IRCOperPassword     :=Config.ReadString ('WormNet','IRCOperPassword', 'Random');
  IRCPassword         :=Config.ReadString ('WormNet','IRCPassword',     IRCDefPassword);
  StealthIP           :=Config.ReadString ('WormNet','StealthIP',       'no.address.for.you');
  NetworkName         :=Config.ReadString ('WormNet','NetworkName',     'MyWormNET');
  MinimumVersion.Str  :=Config.ReadString ('WormNet','MinimumVersion',  '0.0');
  SeenService         :=Config.ReadBool   ('WormNet','SeenService',     false);
  ChatAntiFlood       :=Config.ReadBool   ('WormNet','ChatAntiFlood',   true);
  AntiFloodFactor     :=Config.ReadFloat  ('WormNet','AntiFloodFactor', 1.0);

  VerboseLogging  :=Config.ReadBool   ('Debug','VerboseConsoleLogging', false);
  AllowArbitrary  :=Config.ReadBool   ('Debug','AllowArbitraryPages',   false);

  ServerHost:=StringSection(ServerHost, 0);
  IRCOperPassword:=StringSection(IRCOperPassword, 0);
  IRCPassword:=StringSection(IRCPassword,0);
  StealthIP:=StringSection(StealthIP, 0);

  while Pos(' ',NetworkName) <> 0 do
    NetworkName[Pos(' ',NetworkName)]:=#160;

  if not GetLocalizations(AppLanguage) then
    EventLog('MyWormNET: failed to find the language file - "Languages'+PathDelim+AppLanguage+'.txt"');

  if TextMatch(IRCPassword, 'Default')  then
    IRCPassword:=IRCDefPassword
  else if TextMatch(IRCPassword, 'Random') then
  begin
    IRCPassword:=RandomString(12);
    if IRCPort <> 0 then
      EventLog('MyWormNET: '+Format(L_IRC_RANDOM_PASS, [IRCPassword]));
  end;
  if TextMatch(IRCOperPassword, 'Default') or TextMatch(IRCOperPassword, 'Random') then
  begin
    IRCOperPassword:=RandomString(8);
    if IRCPort <> 0 then
      EventLog('MyWormNET: '+Format(L_IRC_RANDOM_OPERPASS, [IRCOperPassword]));
  end;

  if DirectoryExists('lists') then
  begin
    GetListFromFile(NickBanThreadList, 'lists'+PathDelim+'ban_nicks.txt');
    GetListFromFile(IPBanThreadList, 'lists'+PathDelim+'ban_ips.txt');
    GetListFromFile(WhiteIPThreadList, 'lists'+PathDelim+'white_throttle.txt');
    GetListFromFile(WhiteNickThreadList, 'lists'+PathDelim+'white_nickipauth.txt');
    GetListFromFile(WhitePassauthThreadList, 'lists'+PathDelim+'white_passauth.txt');
  end;
  
  if VerboseLogging or (IRCPort = 0) then
    HTTPtoConsole:=true
  else
    HTTPtoConsole:=false;
  PCS.Leave;
end;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
var
  SX: string;
begin
//  if Copy(S, 1, 1)<>'-' then

  LCS.Enter;
  // logging to disk will work only if the file WNServer.log exists
  TextToFile('['+DateTimeToStr(Now)+'] '+S, 'WNServer.log');

  if (VerboseLogging or Important) and not DiskOnly then
  begin
    // logging to console, if it's enabled
    {$I-}
    SX:=S;
    {$IF Defined(MSWINDOWS) and (CompilerVersion < 20)}
    if CharToOemA(PAnsiChar(S), PAnsiChar(SX)) then
    {$IFEND}
    WriteLn('['+TimeToStr(Now)+'] '+SX);
    {$I+}
  end;
  LCS.Leave;
end;

procedure EventLog(S: string; DiskOnly: Boolean=false);
begin
  Log(S,DiskOnly,true);

  //echo to IRC Admins and Owners
  LogToOper(S);

  if Copy(S, 1, 1)<>'-' then
    S:='['+DateTimeToStr(Now)+'] '+S;

  ECS.Enter;
  // logging to disk will work only if the file EventLog.log exists
  TextToFile(S, 'EventLog.log');
  ECS.Leave;
end;

{$IFDEF MSWINDOWS}

{$I WinSockCodes.inc}

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

{$I stuff.inc}

{function IRCDateTime(T: TDateTime) : Int64;
begin
  Result := Round(T * SecsPerDay) - UnixDateDelta;
end;}

{$IFDEF MSWINDOWS}

function TextDateTime(T: TDateTime; IsUTC: Boolean = false) : string;
var
  Timezone: TTimeZoneInformation;
  Side: Char;
  UTCOffset: TDateTime;
  StrOffset: String;
begin
  EnglishDates;
  GetTimeZoneInformation(Timezone);
  if IsUTC then
    T:=IncMinute(T, -Timezone.Bias);
  UTCOffset := Timezone.Bias/MinsPerDay;
  StrOffset := TimeToStr(UTCOffset);
  StrOffset := Copy(StrOffset,1,Length(StrOffset)-3);
  if UTCOffset>0 then Side:='-'
  else Side:='+';
  Result := FormatDateTime('dddd mmmm d yyyy',T)+' -- '+TimeToStr(T)+' UTC'+Side+StrOffset;
end;

{$IF CompilerVersion >= 20}
function ExecutableTimestamp: TDateTime;
begin
  Result := PImageNtHeaders(HInstance + UIntPtr(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
end;
{$ELSE}
function ExecutableTimestamp: TDateTime;
var
  Timezone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(Timezone);
  Result := IncMinute(FileDateToDateTime(FileAge(ExtractFileName(ParamStr(0)))), Timezone.Bias);
end;
{$IFEND}

{$ELSE}

function TextDateTime(T: TDateTime; IsUTC: Boolean = false) : string;
begin
  Result := FormatDateTime('dddd mmmm d yyyy',T)+' -- '+TimeToStr(T)+' server local time';
end;

function ExecutableTimestamp: TDateTime;
begin
  Result := FileDateToDateTime(FileAge(ExtractFileName(ParamStr(0))));
end;

{$ENDIF}

initialization
  MinimumVersion:=TVersion.Create;
  PCS:=TCriticalSection.Create;
  LCS:=TCriticalSection.Create;
  ECS:=TCriticalSection.Create;
  InitSettings;

finalization
  MinimumVersion.Free;
  LCS.Free;
  ECS.Free;
  PCS.Free;
  Config.Free;

end.

