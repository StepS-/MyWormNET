// WormNet server.

program WNServer;

{$I cDefines.inc}

uses
{$IFDEF OS_MSWIN}
  {$APPTYPE CONSOLE}
  Windows, WinSock,
{$ELSE}
  cthreads, FakeWinSock,
{$ENDIF}
  SysUtils, Base, HTTPServer, IRCServer, WormNATServer, Data;

var
{$IFDEF OS_MSWIN}
  WSA: TWSAData;
{$ENDIF}

begin
  ChDir(ExtractFilePath(ExpandFileName(ParamStr(0))));
  //Log('------------------ '+DateTimeToStr(Now)+' ------------------');
  EventLog('------------------ '+DateTimeToStr(Now)+' ------------------',true);
  EventLog(Format(L_START, [APPVERSION]));

  LoadParams;
  StartupTime:=TextDateTimeNow;

  {$IFDEF OS_MSWIN}
  WSAStartUp(2, WSA);
  {$ENDIF}
  
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
