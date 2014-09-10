// WormNet server.

program WNServer;


uses
  {$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
  Windows, WinSock,
  {$ELSE}
  cthreads, FakeWinSock,
  {$ENDIF}
  SysUtils, Base, HTTPServer, IRCServer, WormNATServer, Data;

{$IFDEF MSWINDOWS}
var
  WSA: TWSAData;
{$ENDIF}

begin
  ChDir(ExtractFilePath(ExpandFileName(ParamStr(0))));
  EventLog('------------------ '+DateTimeToStr(Now)+' ------------------',true);
  EventLog(Format(L_START, [APPVERSION]));

  LoadParams;

  {$IFDEF MSWINDOWS}
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
  while True do
    Sleep(INFINITE);
end.
