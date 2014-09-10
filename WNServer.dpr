// WormNet server.

program WNServer;

{$IF Defined(MSWINDOWS) and (CompilerVersion >= 20)}
  {$IF CompilerVersion >= 25} {$LEGACYIFEND ON} {$IFEND}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  {$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
  Windows, WinSock,
  {$ELSE}
  cthreads, FakeWinSock,
  {$ENDIF}
  SysUtils, Base, HTTPServer, IRCServer, WormNATServer;

{$IFDEF MSWINDOWS}
var
  WSA: TWSAData;
{$ENDIF}

begin
  LoadParams;

  {$IFDEF MSWINDOWS}
  WSAStartUp(MAKEWORD(1,1), WSA);
  {$ENDIF}

  if IRCPort <> 0 then
    StartIRCServer;
  Sleep(50);
  if HTTPPort <> 0 then
    StartHTTPServer;
  Sleep(50);
  if (WormNATPort <> 0) and (IRCPort <> 0) then
    StartWormNATServer;
  while True do
    Sleep(INFINITE);
end.

