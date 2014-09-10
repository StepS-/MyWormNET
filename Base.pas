unit Base;


{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface
const
  APPVERSION = '1.3.7.0';

var
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort: Word;
  AuthSecret, AuthChallenge, AuthAnswer: string;
  IRCOperPassword: string;
  IRCChannel: string;
  StealthIP: string;
  NetworkName: string;
  StartupTime, CreationTime: string;
  VerboseLogging, ForceAuthping, AllowArbitrary: Boolean;

  MaxIRCUsers: Integer=0;
  IRCConnections: Int64=0;

  IPBans, NickBans: array of String;


procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
procedure EventLog(S: string; DiskOnly: Boolean=False);
function WinSockErrorCodeStr(Code: Integer): string;

procedure LoadBanlists;
procedure LoadParams;
procedure LoadAuthping;

function BannedIP(IP: string): Boolean;
function BannedNick(Nick: string): Boolean;

function TextDateTime(T: TDateTime; IsUTC: Boolean = false) : string;
function ExecutableTimestamp: TDateTime;

implementation
uses
{$IFDEF MSWINDOWS}
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
  IRCOperPassword :=Config.ReadString ('WormNet','IRCOperPassword', 'password');
  StealthIP       :=Config.ReadString ('WormNet','StealthIP',      'no.address.for.you');
  NetworkName     :=Config.ReadString ('WormNet','NetworkName',      'MyWormNET');
    StartupTime   :=TextDateTime(Now);
    CreationTime  :=TextDateTime(ExecutableTimestamp, true);
    WormNATPort   :=Config.ReadInteger('WormNet','WormNATPort',     0);

  VerboseLogging  :=Config.ReadBool   ('Debug','VerboseConsoleLogging', false);
  ForceAuthping   :=Config.ReadBool   ('Debug','ForceAuthping',         false);
  AllowArbitrary  :=Config.ReadBool   ('Debug','AllowArbitraryPages',   false);

  ServerHost:=Copy(ServerHost,1,Pos(' ',ServerHost+' ')-1);
  IRCOperPassword:=Copy(IRCOperPassword,1,Pos(' ',IRCOperPassword+' ')-1);
  while Pos(' ',StealthIP) <> 0 do
    StealthIP[Pos(' ',StealthIP)]:=#160;
  while Pos(' ',NetworkName) <> 0 do
    NetworkName[Pos(' ',NetworkName)]:=#160;

  if ForceAuthping then
    LoadAuthping;

end;

procedure Log(S: string; DiskOnly: Boolean=False; Important: Boolean=False);
var
  F: text;
begin
//  if Copy(S, 1, 1)<>'-' then
  S:='['+TimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file WNServer.log exists
  TextToFile(S, ExtractFilePath(ParamStr(0))+'WNServer.log');

  if VerboseLogging or Important then
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

  // echo to IRC OPERs
  LogToOper(S);

  if Copy(S, 1, 1)<>'-' then
    S:='['+DateTimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file EventLog.log exists
  TextToFile(S, ExtractFilePath(ParamStr(0))+'EventLog.log');
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

procedure LoadAuthping;
var Buf: String;
begin
  if not FileExists('authping.txt') then
  begin
    TextToFile('wormnet.team17.com$1', 'authping.txt', true);
    TextToFile('0123456789ABCDEF0123456789ABCDEF01234567', 'authping.txt');
    TextToFile('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', 'authping.txt');
  end;
  Buf:=GetFile('authping.txt')+#10;
  GetLine(Buf,AuthSecret);
  GetLine(Buf,AuthChallenge);
  GetLine(Buf,AuthAnswer);
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

{$IFDEF MSWINDOWS}

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

end.
