unit FakeWinSock;
// WinSock simulator unit for Linux

{$mode delphi}{$H+}

interface

uses
  Sockets, BaseUnix, Unix, UnixType, termio, Classes;

type
  TSocket = Cardinal;
  TSockAddrIn = TInetSockAddr;
  TThreadProc = function (Param: Pointer): Integer; cdecl;
  TIntFunc = Function: Integer;
  u_int = Cardinal;
  TTimeVal = UnixType.timeval;
  PTimeVal = ^TTimeVal;
  TWinFDSet = record
    count: u_int;
    Socket: TSocket;
    end;
  PWinFDSet = ^TWinFDSet;

const
  SOCKET_ERROR = -1;
  INFINITE = $7FFFFFFF;
  IPPROTO_TCP = 6;
  INVALID_SOCKET = 0;
  FIONREAD = termio.FIONREAD;

var
  WSAGetLastError: TIntFunc = @fpgeterrno;

function ioctlsocket(Socket: TSocket; Message: Integer; var Parameter): Integer;
function CreateThread(Bla: Pointer; Bla2: Integer; Proc: TThreadProc; Param: Pointer; Bla3: Integer; var ThreadID: Cardinal): Integer;

function inet_ntoa(Addr: in_addr): string;
function inet_addr(Addr: string): cardinal;
function select (N:cint; readfds,writefds,exceptfds:PWinFDSet; TimeOut:PTimeVal):cint;  // HACK
function bind (s:cint; var addrx : tsockaddrin; addrlen : tsocklen):cint;
function listen (s:cint; backlog : cint):cint;
function accept (s:cint; addrx : psockaddr; addrlen : psocklen):cint;
function socket (domain:cint; xtype:cint; protocol: cint):cint;
function recv (s:cint; buf: char; len: size_t; flags: cint):ssize_t;
function send (s:cint; msg: char; len:size_t; flags:cint):ssize_t;

implementation

function inet_ntoa(Addr: in_addr): string;
begin
  Result:=NetAddrToStr(Addr);
end;

function inet_addr(Addr: string): cardinal;
begin
  Result:=StrToNetAddr(Addr).s_addr;
end;

function ioctlsocket(Socket: TSocket; Message: Integer; var Parameter): Integer;
begin
  Result:=BaseUnix.FpIOCtl(Socket, Message, @Parameter);
end;

function select (N:cint; readfds,writefds,exceptfds:PWinFDSet; TimeOut:PTimeVal):cint;  // HACK
var
  r: TFDSet;
begin
  fpFD_ZERO(r);
  fpFD_SET(readfds.Socket, r);
  Result:=FPSelect(N, @r,nil,nil, (TimeOut.tv_sec*1000)+(TimeOut.tv_usec div 1000));
end;

function bind (s:cint; var addrx : tsockaddrin; addrlen : tsocklen):cint;
begin
  Result:=fpbind(s, @addrx, addrlen);
end;

function listen (s:cint; backlog : cint):cint;
begin
  Result:=fplisten(s, backlog);
end;

function accept (s:cint; addrx : psockaddr; addrlen : psocklen):cint;
begin
  Result:=fpaccept(s, addrx, addrlen);
end;

function socket (domain:cint; xtype:cint; protocol: cint):cint;
begin
  Result:=fpsocket(Domain,xtype,ProtoCol);
end;

function recv (s:cint; buf: char; len: size_t; flags: cint):ssize_t;
begin
  Result:=fprecv(S,@Buf,Len,Flags);
end;

function send (s:cint; msg: char; len:size_t; flags:cint):ssize_t;
begin
  Result:=fpSend(S,@msg,len,flags);
end;

type
  TCustomThread=class(TThread)
    public
    FProc: TThreadProc;
    FParam: Pointer;
    procedure Execute; override;
    end;

procedure TCustomThread.Execute;
begin
  FreeOnTerminate:=True;
  FProc(FParam);
end;

function CreateThread(Bla: Pointer; Bla2: Integer; Proc: TThreadProc; Param: Pointer; Bla3: Integer; var ThreadID: Cardinal): Integer;
begin
  Result:=1;
  ThreadID:=1;
  with TCustomThread.Create(True) do
    begin
    FProc:=Proc;
    FParam:=Param;
    Resume;
    end;

end;

end.

