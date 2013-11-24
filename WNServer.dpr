// WormNet server.

program WNServer;

{$IFNDEF VER150}
{$LEGACYIFEND ON}
{$ENDIF}

uses
{$IF Defined(Win32) OR Defined(Win64)}
  {$APPTYPE CONSOLE}
  Windows, WinSock,
{$ELSE}
  cthreads, FakeWinSock,
{$IFEND}
  SysUtils, Base, IniFiles, HTTPServer, IRCServer, WormNATServer, Data;

var
  Config: TMemIniFile;
{$IF Defined(Win32) OR Defined(Win64)}
  WSA: TWSAData;
{$IFEND}

begin
  ChDir(ExtractFilePath(ExpandFileName(ParamStr(0))));
  //Log('------------------ '+DateTimeToStr(Now)+' ------------------');
  EventLog('------------------ '+DateTimeToStr(Now)+' ------------------',true);
  EventLog('Starting MyWormNET version '+APPVERSION+'.');

  Config := TMemIniFile.Create(ExtractFilePath(ParamStr(0))+'WNServer.ini');
  ServerHost      :=Config.ReadString ('WormNet','ServerHost',     'localhost');
  IRCPort         :=Config.ReadInteger('WormNet','IRCPort',               6667);
  HTTPPort        :=Config.ReadInteger('WormNet','HTTPPort',                80);
  WormNATPort     :=Config.ReadInteger('WormNet','WormNATPort',          17018);
  VerboseLogging  :=Config.ReadInteger('WormNet','VerboseLogging',           0);
  IRCOperPassword :=Config.ReadString ('WormNet','IRCOperPassword', 'password');
  StealthIP       :=Config.ReadString ('WormNet','StealthIP',      'no.address.for.you');
  NetworkName     :=Config.ReadString ('WormNet','NetworkName',      'MyWormNET');

  StartupTime:=TextDateTimeNow;

  {$IF Defined(Win32) OR Defined(Win64)}
  WSAStartUp(2, WSA);
  {$IFEND}
  
  LoadBanlists;

  if IRCPort>0 then
    StartIRCServer;
  Sleep(50);
  if HTTPPort>0 then
    StartHTTPServer;
  Sleep(50);
  if WormNATPort>0 then
    StartWormNATServer;
  Sleep(INFINITE);
end.
