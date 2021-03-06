unit WormNATServer;

{-------------------------------------------------------------------------------
| A proxy for WormNAT routing.
| (C) CyberShadow - 2006
| (C) StepS - 2013-2014
|-------------------------------------------------------------------------------
| FEATURES:
|
| - Nostalgia
| - Almost totally obsolete, only kept for history
| - Disabled by default
| - Probably broken some updates ago
| - Cooperates with the IRC server for WormNAT users to join WormNAT games
| - More details about this artifact: http://worms.thecybershadow.net/wormnat/
-------------------------------------------------------------------------------}

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  Classes, Types, IRCServer, Localization;

type
  TLinkType=(ltServer, ltClient);

  TLink=class (TThread)          // pumps data between two sockets
    LinkType: TLinkType;
    ServerNickname, ClientNickname: string;
    ServerAddress, ClientAddress: string;
    ServerSocket, ClientSocket: TSocket;
    procedure Execute; override;
    procedure ResumeThread;
    end;

var
  LinkThreadList: TThreadList;
  ThreadID: Cardinal = 0;

procedure StartWormNATServer;
procedure Freedom;
function PrepareLink(Server, Client: TUser): TLink;

implementation
uses
  Base, HTTPServer, SysUtils;

procedure TLink.Execute;
var
  SA: AnsiString;
  R, Bytes: Integer;
  ReadSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
begin
  try
    Log('[WormNAT] '+L_WORMNAT_START_LINK+' '+ClientNickname+' ('+ClientAddress+') -> '+ServerNickname+' ('+ServerAddress+').');
    repeat
      // Client -> Server
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ClientSocket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;  // 10 ms
        R:=select(ClientSocket+1, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create(L_ERROR_CLIENT_SELECT+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ClientSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create(L_ERROR_CLIENT_SELECT+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if Bytes=0 then  // software disconnect
          raise Exception.Create(L_ERROR_CLIENT_SELECT+' (Graceful disconnect).');

        SetLength(SA, Bytes);
        R:=recv(ClientSocket, SA[1], Length(SA), 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create(L_ERROR_CLIENT_SELECT+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        SetLength(SA, R);
        send(ServerSocket, SA[1], Length(SA), 0);
      until False;

      // Server -> Client
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=ServerSocket;
        R:=select(0, @ReadSet, nil, nil, @TimeVal);
        if R=SOCKET_ERROR then
          raise Exception.Create(L_ERROR_SERVER_SELECT+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if (ReadSet.count=0)or(R=0) then
          Break;         // nothing to read

        R:=ioctlsocket(ServerSocket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          raise Exception.Create(L_ERROR_SERVER_CONNECTION+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');

        if Bytes=0 then  // software disconnect
          raise Exception.Create(L_ERROR_SERVER_CONNECTION+' (Graceful disconnect).');

        SetLength(SA, Bytes);
        R:=recv(ServerSocket, SA[1], Length(SA), 0);
        if(R=0)or(R=SOCKET_ERROR)then
          raise Exception.Create(L_ERROR_SERVER_CONNECTION+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        SetLength(SA, R);
        send(ClientSocket, SA[1], Length(SA), 0);
      until False;
    until False;
  except
    on E: Exception do
      EventLog(L_WORMNAT_LINK_ERROR+' '+ClientNickname+': '+E.Message);
    end;
  closesocket(ServerSocket);
  closesocket(ClientSocket);

  LinkThreadList.Remove(Self);
  FreeOnTerminate:=True;
end;

procedure TLink.ResumeThread;
begin
  {$IFDEF MSWINDOWS} {$IF CompilerVersion >= 21}
  Start;
  {$ELSE} Resume;
  {$IFEND}
  {$ELSE} Start; {$ENDIF}
end;

// ***************************************************************

function PrepareLink(Server, Client: TUser): TLink;
begin
  Result:=TLink.Create(True);
  Result.ServerNickname:=Server.Nickname;
  Result.ServerAddress:=Server.ConnectingFrom;
  Result.ClientNickname:=Client.Nickname;
  Result.ClientAddress:=Client.ConnectingFrom;
  Result.ServerSocket:=0;
  Result.ClientSocket:=0;
  LinkThreadList.Add(Result);
end;

procedure Freedom;
var
  I: Integer;
  LinkList: TList;
begin
  LinkList:=LinkThreadList.LockList;
  for I := LinkList.Count-1 downto 0 do
  begin
    TLink(LinkList[I]).Free;
    LinkList.Remove(LinkList[I]);
  end;
  LinkThreadList.UnlockList;
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  ServicePort: Integer;
  I, T: Integer;
  B: Boolean;
  LinkList: TList;
begin
  Result:=0;
  LinkThreadList.Clear;

  ServicePort:=WormNATPort;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr('0.0.0.0');
  service.sin_port := htons( ServicePort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
  begin
    EventLog('[WormNAT] '+Format(L_ERROR_BIND, [ServicePort, WinSockErrorCodeStr(WSAGetLastError)]));
    Exit;
  end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
  begin
    EventLog('[WormNAT] '+Format(L_ERROR_BIND, [ServicePort, WinSockErrorCodeStr(WSAGetLastError)]));
    Exit;
  end;
  EventLog('[WormNAT] '+Format(L_SERVICE_LISTENING, [ServicePort]));

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := CustomAccept(m_socket, @incoming, @T, @ServicePort);
    if AcceptSocket=INVALID_SOCKET then
      Sleep(5)
    else if AcceptSocket<>0 then
    begin
      T:=SizeOf(incoming);
      EventLog('[WormNAT] '+Format(L_CONNECTION_ESTABLISHED, [inet_ntoa(incoming.sin_addr)]));

      B:=False;
      LinkList:=LinkThreadList.LockList;
      for I:=0 to LinkList.Count-1 do with TLink(LinkList[I]) do
      begin
        if(ServerAddress=String(inet_ntoa(incoming.sin_addr)))and(ServerSocket=0) then
        begin
          ServerSocket:=AcceptSocket;
          if ClientSocket<>0 then
            ResumeThread;
          B:=True;
        end;
        if(ClientAddress=String(inet_ntoa(incoming.sin_addr)))and(ClientSocket=0) then
        begin
          ClientSocket:=AcceptSocket;
          if ServerSocket<>0 then
            ResumeThread;
          B:=True;
        end;
      end;
      LinkThreadList.UnlockList;
      if not B then
      begin
        Log('[WormNAT] '+L_ERROR+': '+L_WORMNAT_UNEXPECTED+' '+String(inet_ntoa(incoming.sin_addr)));
        closesocket(AcceptSocket);
      end;
    end;
  until False;
end;

procedure StartWormNATServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

initialization
  LinkThreadList:=TThreadList.Create;

finalization
  LinkThreadList.Free;

end.

