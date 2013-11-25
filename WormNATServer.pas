unit WormNATServer;
// a proxy for WormNAT routing

{$I cDefines.inc}

interface
uses
{$IFDEF OS_MSWIN}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  Classes, IRCServer;

type
  TLinkType=(ltServer, ltClient);

  TLink=class (TThread)          // pumps data between two sockets
    LinkType: TLinkType;
    ServerNickname, ClientNickname: string;
    ServerAddress, ClientAddress: string;
    ServerSocket, ClientSocket: TSocket;
    procedure Execute; override;
    end;

var
  Links: array of TLink;

procedure StartWormNATServer;
procedure PrepareLink(Server, Client: TUser);

implementation
uses
  Base, SysUtils;

procedure TLink.Execute;
var
  SA: AnsiString;
  R, Bytes, I, N: Integer;
  ReadSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
begin
  try
    Log('[WormNAT] Initializing link between '+ClientNickname+' ('+ClientAddress+') -> '+ServerNickname+' ('+ServerAddress+').');
    repeat
      // Client -> Server
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ClientSocket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;  // 10 ms
        R:=select(ClientSocket+1, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create('Client select() error ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ClientSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create('Client connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if Bytes=0 then  // software disconnect
          raise Exception.Create('Client connection error (Graceful disconnect).');

        SetLength(SA, Bytes);
        R:=recv(ClientSocket, SA[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create('Client connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        SetLength(SA, R);
        send(ServerSocket, SA[1], Length(SA), 0);
      until False;

      // Server -> Client
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ServerSocket;
        R:=select(0, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create('Server select() error ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ServerSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create('Server connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if Bytes=0 then  // software disconnect
          raise Exception.Create('Server connection error (Graceful disconnect).');

        SetLength(SA, Bytes);
        R:=recv(ServerSocket, SA[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create('Server connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        SetLength(SA, R);
        send(ClientSocket, SA[1], Length(SA), 0);
      until False;
    until False;
  except
    on E: Exception do
      EventLog('Error in link with '+ClientNickname+': '+E.Message);
    end;
  closesocket(ServerSocket);
  closesocket(ClientSocket);

  // TODO: add some sync lock or something here
  N:=-1;
  for I:=0 to Length(Links)-1 do
    if Links[I]=Self then
      N:=I;
  for I:=N to Length(Links)-2 do
    Links[I]:=Links[I+1];
  SetLength(Links, Length(Links)-1);
  FreeOnTerminate:=True;
end;

// ***************************************************************

procedure PrepareLink(Server, Client: TUser);
var
  Link: TLink;
begin
  Link:=TLink.Create(True);
  Link.ServerNickname:=Server.Nickname;
  Link.ServerAddress:=Server.ConnectingFrom;
  Link.ClientNickname:=Client.Nickname;
  Link.ClientAddress:=Client.ConnectingFrom;
  Link.ServerSocket:=0;
  Link.ClientSocket:=0;
  SetLength(Links, Length(Links)+1);
  Links[Length(Links)-1]:=Link;
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  I, T: Integer;
  B: Boolean;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr('0.0.0.0');
  service.sin_port := htons( WormNATPort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
    begin
    EventLog('[WormNAT] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    EventLog('[WormNAT] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  EventLog('[WormNAT] Listening on port '+IntToStr(WormNATPort)+'.');

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if (AcceptSocket<>INVALID_SOCKET) then
    begin
      if not BannedIP(String(inet_ntoa(incoming.sin_addr))) then
      begin
        T:=SizeOf(incoming);
        EventLog('[WormNAT] Connection established from '+inet_ntoa(incoming.sin_addr));

        B:=False;
        for I:=0 to Length(Links)-1 do
          with Links[I] do
          begin
            if(ServerAddress=String(inet_ntoa(incoming.sin_addr)))and(ServerSocket=0) then
            begin
              ServerSocket:=AcceptSocket;
              if ClientSocket<>0 then
                {$IFDEF DELPHI2010_UP}
                Start;
                {$ELSE}
                Resume;
                {$ENDIF}
              B:=True;
            end;
            if(ClientAddress=String(inet_ntoa(incoming.sin_addr)))and(ClientSocket=0) then
            begin
              ClientSocket:=AcceptSocket;
              if ServerSocket<>0 then
                {$IFDEF DELPHI2010_UP}
                Start;
                {$ELSE}
                Resume;
                {$ENDIF}
              B:=True;
            end;
          end;

        if not B then
        begin
          EventLog('[WormNAT] Error: Unexpected connection from '+inet_ntoa(incoming.sin_addr));
          closesocket(AcceptSocket);
        end;
      end
      else
      begin
        EventLog('Rejected request from banned IP '+inet_ntoa(incoming.sin_addr)+' to port '+IntToStr(WormNATPort)+'.');
        closesocket(AcceptSocket);
        Sleep(5);
      end;
    end
    else
      Sleep(5);
  until False;
end;

var 
  ThreadID: Cardinal = 0;

procedure StartWormNATServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

end.
