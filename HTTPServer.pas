unit HTTPServer;
// simplistic HTTP server

{$I cDefines.inc}

interface
uses
  SysUtils, Classes,
{$IFDEF OS_MSWIN}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  IRCServer;

type
  TRequest=class(TThread)
    Socket: TSocket;
    ConnectingFrom: string;
    FileName: string;
    Parameters: TStringList;
    procedure Execute; override;
    procedure SendLn(S: string);
    end;

  TGame=record
    Created: TDateTime;
    Name, Password, Loc, PassNeeded, Chan, LType: string;
    HosterNickname, HosterAddress, HostedFrom: string;
    Scheme: string;
    GameID: Integer;
    end;

var
  Games: array of TGame;
  GameCounter: Integer=0;

procedure StartHTTPServer;

implementation
uses
  Base, Data, DateUtils;

procedure CleanUpGames;
var
  I, J: Integer;
begin
  for I:=Length(Games)-1 downto 0 do
    if MinutesBetween(Games[I].Created, Now)>4 then
      begin
      EventLog(Format(L_GAME_CLOSED_TIMEOUT, [Games[I].HosterNickname, Games[I].Name]));
      for J:=I to Length(Games)-2 do
        Games[J]:=Games[J+1];
      SetLength(Games, Length(Games)-1);
      end;
end;

{$I mime.inc}

procedure TRequest.Execute;
var
  BufferA, SA: AnsiString;
  Buffer, S, Str, Headers, Body: String;
  I, J, N, R, Bytes: Integer;
//User: TUser;
  Channel: TChannel;
  Game: TGame;
begin
  try
    Buffer:='';
    BufferA:='';
    repeat
      BufferA:='';
      SA:='';
      R:=ioctlsocket(Socket, FIONREAD, Bytes);
      if R=SOCKET_ERROR then
      begin
        Log('[HTTP] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        Exit;
      end;
      if Bytes=0 then 
      begin
        Sleep(10);
        Continue;
      end;
      SetLength(SA, Bytes);
      R:=recv(Socket, SA[1], Bytes, 0);
      if(R=0)or(R=SOCKET_ERROR)then
      begin
        Log('[HTTP] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        Exit;
      end;
      SetLength(SA, R);
      BufferA := BufferA + SA;
      Buffer := String(BufferA);
      S := String(SA);
    until Copy(Buffer, Length(Buffer)-3, 4)=#13#10#13#10;      // ends with an empty line

    GetLine(Buffer, S);
    Log('[HTTP] '+ConnectingFrom+' '+S);
    if Copy(S, 1, 4)<>'GET ' then
      raise Exception.Create('Only GET requests are supported');
    Delete(S, 1, 4);
    S:=Copy(S, 1, Pos(' ', S+' ')-1);
    if LowerCase(Copy(S, 1, 7))='http://' then
      begin
      Delete(S, 1, 7);
      Delete(S, 1, Pos('/', S)-1);
      end;
    if Copy(S, 1, 2)='//' then   // workaround to some dumb bug
      begin
      Delete(S, 1, 2);
      Delete(S, 1, Pos('/', S));
      end; 
    while Copy(S, 1, 1)='/' do
      Delete(S, 1, 1);
    if Copy(S, 1, 15)='wormageddonweb/' then
      Delete(S, 1, 15);
    FileName:=Copy(S, 1, Pos('?', S+'?')-1);
    Delete(S, 1, Pos('?', S));
    S:=S+'&';
    Parameters:=TStringList.Create;
    Parameters.NameValueSeparator:='=';
    while S<>'' do
    begin
      Parameters.Add(Copy(S, 1, Pos('&', S)-1));
      Delete(S, 1, Pos('&', S));
    end;

    //while GetLine(Buffer, S) do
    //  WriteLn('> ' + S);

    Headers:='HTTP/1.0 200 OK'#13#10;
    Headers:=Headers+'X-Powered-By: MyWormNET'#13#10;
//    Headers:=Headers+'Connection: close'#13#10;
//    Headers:=Headers+'X-Test: BlaBla'#13#10;
    Body:='';
    if FileName='Login.asp' then
    begin
      if ConnectingFrom='127.0.0.1' then
        Body:='<CONNECT 127.0.0.1>'
      else
        Body:='<CONNECT '+ServerHost+'>';
      Body:=Body+#13#10'<MOTD>'+GetFile('news.txt')+#13#10'</MOTD>';
    end
    else
    if FileName='Rankings.asp' then
      Body:='<CENTER>Sorry, this server doesn''t support rankings.</CENTER>'
    else
    if FileName='ProcessGameResult.asp' then
      Body:='<NOTHING>'
    else
    if FileName='RequestChannelScheme.asp' then
    begin
      Str:='#'+Parameters.Values['Channel'];
      Channel:=ChannelByName(Str);
      if Channel <> nil then
        Body:='<SCHEME='+Channel.Scheme+'>'#10
      else
        Body:='<SCHEME=Pf,Be>'#10;
    end
    else
    // Cmd=Create&Name=ßCyberShadow-MD&HostIP=cybershadow.no-ip.org&Nick=CyberShadow-MD&Pwd=123&Chan=AnythingGoes&Loc=40&Type=0 HTTP/1.0
    if FileName='Game.asp' then
      if Parameters.Values['Cmd']='Create' then
        begin
        for I:=0 to Length(Games)-1 do
          if Games[I].HostedFrom = ConnectingFrom then
          begin
            EventLog(Format(L_GAME_CLOSED_ANTIFLOOD, [Games[I].HosterNickname, Games[I].Name, ConnectingFrom]));
            for J:=I to Length(Games)-2 do
              Games[J]:=Games[J+1];
            SetLength(Games, Length(Games)-1);
            Break;
          end;

        Game.Name:=Parameters.Values['Name'];
        if Length(Game.Name) > 29 then Game.Name := Copy(Game.Name, 1, 29);
          Game.Password:=Parameters.Values['Pwd'];
        if (Game.Password <> '') then Game.PassNeeded:='1'
        else Game.PassNeeded:='0';
        Game.LType:=Parameters.Values['Type'];
        Game.Chan:=Parameters.Values['Chan'];
        Game.Loc:=Parameters.Values['Loc'];
        Game.HosterNickname:=Parameters.Values['Nick'];
        Game.HosterAddress:=Parameters.Values['HostIP'];
        Game.HostedFrom:=ConnectingFrom;
        Str:='#'+Game.Chan;
        Channel:=ChannelByName(Str);
        if Channel <> nil then
          if Pos('Tf',Channel.Scheme) = 0 then
          begin
            Game.Scheme:=Channel.Scheme;
            Inc(GameCounter);
            Game.GameID:=GameCounter;
            Game.Created:=Now;

            SetLength(Games, Length(Games)+1);
            Games[Length(Games)-1]:=Game;

            EventLog(Format(L_GAME_CREATED_INFO, [Game.HosterNickname, Game.Name, Game.HosterAddress, Game.HostedFrom]));

            Headers:=Headers+'SetGameId: : '+IntToStr(Game.GameID)+#13#10;
            Body:='<html>'#10'<head><title>Object moved</title></head>'#10'<body>'#10'<h1>Object moved</h1>'#10'This object may be found <a href="/wormageddonweb/GameList.asp?Channel='+Game.Chan+'">here</a>.'#10'</body>'#10'</html>';
            // The string above is for compatibility with Wheat Snooper, otherwise it can't host: Yes, I know, it's quite stupid.
          end
          else
          begin
            EventLog(Format(L_GAME_FAIL_NONGAMING, [Game.HosterNickname, Game.Name, Game.HosterAddress, Channel.Name, Game.HostedFrom]));
            Body:='<NOTHING>';
          end
        else
          begin
          EventLog(Format(L_GAME_FAIL_NONEXISTENT_CHAN, [Game.HosterNickname, Game.Name, Game.HosterAddress, Game.Chan, Game.HostedFrom]));
          Body:='<NOTHING>';
          end;
        end
      else
      if Parameters.Values['Cmd']='Close' then
      begin
        N:=-1;
        for I:=0 to Length(Games)-1 do
          if IntToStr(Games[I].GameID)=Parameters.Values['GameID'] then
            if (ConnectingFrom = Games[I].HostedFrom) or (ConnectingFrom = '127.0.0.1') then
            begin
              Game:=Games[I];
              for J:=I to Length(Games)-2 do
                Games[J]:=Games[J+1];
              SetLength(Games, Length(Games)-1);
              N:=I;
              Break;
            end
            else
            begin
              EventLog(Format(L_GAME_FAIL_SABOTAGE, [ConnectingFrom, Games[I].HosterNickname, Parameters.Values['GameID'], Games[I].HostedFrom]));
              Break;
            end;
        if N=-1 then
          begin
          //raise Exception.Create('No such game');
          //EventLog(Format(L_GAME_FAIL_NONEXISTENT_GAME, [ConnectingFrom, Parameters.Values['GameID']]));
          end
        else
          begin
        {  for I:=0 to Length(Users)-1 do
           if Users[I].InChannel then
              Users[I].SendLn(':'+ServerHost+' NOTICE '+IRCChannel+' :'+Game.HosterNickname+'''s game "'+Game.Name+'" has closed.');}
          EventLog(Format(L_GAME_CLOSED_GRACEFULLY, [Game.HosterNickname, Game.Name]));
          end;
        end
      else
        Body:='<NOTHING>'
      {
      if Parameters.Values['Cmd']='Failed' then        // ?
        begin
        Body:='<NOTHING>';
        end
      else
        raise Exception.Create('Unknown game command - '+Parameters.Values['Cmd'])
      }
    else
    if FileName='GameList.asp' then
      begin
      CleanUpGames;
      Body:=Body+'<GAMELISTSTART>'#10;
      Str:=Parameters.Values['Channel'];
      for I:=0 to Length(Games)-1 do
        with Games[I] do
          if Str=Chan then
            Body:=Body+'<GAME '+Name+' '+HosterNickname+' '+HosterAddress+' '+Loc+' 1 '+PassNeeded+' '+IntToStr(GameID)+' '+LType+'><BR>'#10;
      Body:=Body+'<GAMELISTEND>'#10;
      end
    else
    if FileName='UpdatePlayerInfo.asp' then
      // ignore
    else
      begin
      for I:=Length(FileName) downto 1 do
        if(FileName[I]='/')and(PathDelim='\') then
          FileName[I]:=PathDelim
        else
        if FileName[I]='%' then
          begin
          FileName[I]:=Chr(StrToInt('$'+Copy(FileName, I+1, 2)));
          Delete(FileName, I+1, 2);
          end;
      if Pos('..', FileName)+Pos(PathDelim+PathDelim, FileName)<>0 then
        raise Exception.Create('hmm hmm hmm');
      if(FileName='')or(FileName[Length(FileName)]=PathDelim)then
        FileName:=FileName+'index.html';
      if FileExists('wwwroot'+PathDelim+FileName) then
        begin
        Log('[HTTP] '+ConnectingFrom+' '+L_HTTP_FILE_SENDING+' '+FileName);
        S:='application/octet-stream';
        for I:=1 to High(MimeTypes) do
          if '.'+MimeTypes[I].Extension=ExtractFileExt(FileName) then
            S:=MimeTypes[I].MimeType;
        Headers:=Headers+'Content-Type: '+S+#13#10;
        Body:=GetFile('wwwroot'+PathDelim+FileName);
        end
      else
        raise Exception.Create('"File" not found - '+FileName);
      end;

    Headers:=Headers+'Content-Length: '+IntToStr(Length(Body))+#13#10;
    S:=Headers+#13#10+Body;
    SendLn(S);
    Parameters.Free;
  except
    on E: Exception do
      try
        Log('[HTTP] '+L_ERROR_WITH+' '+ConnectingFrom+' : '+E.Message);
        S:=Headers+'Error: : '+E.Message+#13#10#13#10+'Error: '+E.Message;
        SendLn(S);
      except
        end;
    end;
  closesocket(Socket);
  FreeOnTerminate:=True;
end;


procedure TRequest.SendLn(S: string);
var
  AStr: AnsiString;
begin
  AStr:=AnsiString(S);
  AStr:=AStr+#13#10;
  if send(Socket, AStr[1], Length(AStr), 0)<>Length(AStr) then
    Log('[HTTP] > '+L_FAILED+' ('+WinSockErrorCodeStr(WSAGetLastError)+')');
end;

// ***************************************************************

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  T: Integer;
  Request: TRequest;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr( '0.0.0.0' );
  service.sin_port := htons( HTTPPort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
  begin
    EventLog('[HTTP] '+L_BIND_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
  end;
  if listen( m_socket, 50 )=SOCKET_ERROR then
  begin
    EventLog('[HTTP] '+L_BIND_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
  end;
  EventLog('[HTTP] '+L_LISTENING+' '+IntToStr(HTTPPort)+'.');

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if (AcceptSocket<>INVALID_SOCKET) then
    begin
      if not BannedIP(String(inet_ntoa(incoming.sin_addr))) then
      begin
        T:=SizeOf(incoming);
        Log('[HTTP] '+L_CONNECTION_ESTABLISHED+' '+inet_ntoa(incoming.sin_addr));

        Request:=TRequest.Create(true);
        Request.Socket:=AcceptSocket;
        Request.ConnectingFrom:=String(inet_ntoa(incoming.sin_addr));
        {$IFNDEF DELPHI2009_DOWN}
        Request.Start;
        {$ELSE}
        Request.Resume;
        {$ENDIF}
      end
      else
      begin
        EventLog(Format(L_REQUEST_REJECTED, [inet_ntoa(incoming.sin_addr), IntToStr(HTTPPort)]));
        closesocket(AcceptSocket);
        Sleep(1);
      end;
    end
    else
      Sleep(1);
  until False;
end;

var
  ThreadID: Cardinal = 0;

procedure StartHTTPServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;


end.
