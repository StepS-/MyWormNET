unit HTTPServer;
// simplistic HTTP server

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface
uses
  SysUtils, Classes,
  Windows, WinSock,
{$IFDEF MSWINDOWS}
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
    procedure SendLn(S: string; Logging: Boolean=true);
      procedure ResumeThread;
    end;

  TGame=record
    Created: TDateTime;
    Name, Password, Loc, Chan, LType: string;
    HosterNickname, HosterAddress, HostedFrom: string;
    PassNeeded: char;
    Scheme: string;
    GameID: Integer;
    end;

var
  GameThreadList: TThreadList;
  GameCounter: Integer=1000000;

procedure StartHTTPServer;
function AspPhp(S, PageName: string): Boolean;

implementation
uses
  Base, Data, DateUtils;

procedure CleanUpGames;
var
  I: Integer;
  GameList: TList;
begin
  GameList:=GameThreadList.LockList;
  for I:=GameList.Count-1 downto 0 do with TGame(GameList[I]) do
    if Age > 300 then
    begin
      EventLog(Format(L_GAME_CLOSED_TIMEOUT, [HosterNickname, Name]));
      GameList.Remove(GameList[I]);
      Free;
    end;
  GameThreadList.UnlockList;
end;

{$I mime.inc}

procedure TRequest.Execute;
var
  BufferA, SA: AnsiString;
  Buffer, S, Str, CmdParam, IParam, Target: String;
  Headers, UserAgent, ClientVersion, Body: String;
  I, J, N, R, Bytes: Integer;
  ExternalFile: Boolean;
//User: TUser;
  Channel: TChannel;
  GameList: TList;
  Game: TGame;
begin
  try
    Buffer:='';
    BufferA:='';
    repeat
      BufferA:=ansistring(Buffer);
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
    until Copy(BufferA, Length(BufferA)-3, 4)=#13#10#13#10;      // ends with an empty line

    GetLine(Buffer, S);
    Log('[HTTP] '+ConnectingFrom+' '+S);
    if Copy(S, 1, 4)<>'GET ' then
      raise Exception.Create('Only GET requests are supported');
    Delete(S, 1, 4);
    S:=Copy(S, 1, Pos(' ', S+' ')-1);
    S:=EncodeURI(S);
    if TextMatch(Copy(S, 1, 7),'http://') then
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

    if (ConnectingFrom='127.0.0.1') or (Pos('192.168.',ConnectingFrom) = 1) or (Pos('10.',ConnectingFrom) = 1) then
      Target:=ConnectingFrom
    else
      Target:=ServerHost;

    UserAgent:='Unknown';
    ClientVersion:='0.0';
    while GetLine(Buffer,S) do
      if Pos('User-Agent: ',S) = 1 then
      begin
        Delete(S, 1, 12);
        UserAgent:=Copy(S, 1, Length(S));
        S:=UserAgent;
        if Pos('T17Client',UserAgent) = 1 then
        begin
          Delete(S, 1, 10);
          if S <> '' then
            ClientVersion:=Copy(S, 1, Pos(' ',S+' ')-1)
          else
            ClientVersion:='1.0';
        end;
        Break;
      end;

    Headers:='HTTP/1.1 200 OK'#13#10;
    Headers:=Headers+'Server: MyWormNET/'+APPVERSION+#13#10;
    Headers:=Headers+'X-Powered-By: MyWormNET'#13#10;
//  Headers:=Headers+'Pragma: no-cache'#13#10;
//  Headers:=Headers+'Vary: Accept-Encoding'#13#10;
//  Headers:=Headers+'X-Test: BlaBla'#13#10;
    Body:='';
    if AspPhp(FileName,'Login') or AspPhp(FileName,'Connect')then
    begin
      if VersionThisNewer(ClientVersion,'3.6.29.29') then
      begin
        Body:='<CONNECT '+Target+'>';
        Body:=Body+#13#10'<MOTD>'+GetTextFile('news.xml')+'</MOTD>'#10;
      end
      else
        if ClientVersion = '2.0' then
          Body:='<CONNECT '+Target+' IRCPORT='+IntToStr(IRCPort)+' IRCUSER=WWP IRCPASS='+IRCPassword+'>'
      else
        Body:='<CONNECT '+Target+'>'#10;
    end
    else
    if AspPhp(FileName,'UpdatePlayerInfo') then
      if ClientVersion = '2.0' then
        Body:='<CONNECT '+Target+' IRCPORT='+IntToStr(IRCPort)+' IRCUSER=WWP IRCPASS='+IRCPassword+'>'
      else
        Body:='<NOTHING>'#10
    else
    if AspPhp(FileName,'WelcomeLoginForm') then
    begin
      if ClientVersion = '2.0' then
      begin
        Body:='<CHECK AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=>'#10;
        Body:=Body+'<WEBADDRESS /wwpweb/>'#10;
        Body:=Body+'<EXTENSION .php>'#10;
        Body:=Body+'<FONT Size=1 Colour=0>                           Welcome to '+NetworkName+'<BR></FONT>'#10;
        Body:=Body+'<br><br><FONT Size=0 Colour=0>                                 You can use any username / password to log in<BR></FONT>'#10;
        Body:=Body+'<FONT Size=3 Colour=3>                                                               Your IP address is detected as: '+ConnectingFrom+'<BR></FONT>'#10;
        Body:=Body+'<br>'#10;
        Body:=Body+'<a href="/wwpweb/LoginForm.php"><FONT Size=0>                                                                        Login<BR></FONT>'#10;
        Body:=Body+'<br></a>'#10;
      end
      else
      if VersionThisNewer(ClientVersion,'3.6.29.10') then
      begin
        Body:=Body+'<CENTER><FONT SIZE=3 COLOR="White">Welcome to '+NetworkName+'</FONT></CENTER><BR><BR><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=2 COLOR=0>You can use any username / password to log in</FONT></CENTER><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=1 COLOR="Silver">Your IP address is detected as: '+ConnectingFrom+'</FONT></CENTER><BR><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=2><A HREF="/wwpweb/LoginForm.php">Login</A></FONT></CENTER>'#10;
      end
      else
        Body:='<FONT SIZE=2><A HREF="/wormageddonweb/LoginForm.asp">Login</A></FONT>'#10;
    end
    else
    if AspPhp(FileName,'SelectServer') or AspPhp(FileName,'LoginForm') then
      Body:='<SHOWLOGIN>'#10
    else
    if AspPhp(FileName,'RequestAuth') then
      Body:='<ANSWER '+AuthAnswer+'>'#10
    else
    if AspPhp(FileName,'RequestChannelScheme') then
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
    if AspPhp(FileName,'Game') then
    begin
      CmdParam:=Parameters.Values['Cmd'];
      if TextMatch(CmdParam,'Create') then
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

        Game.Name:=DecodeURI(Parameters.Values['Name']);
        if ClientVersion = '2.0' then Game.Name:='[WWP]'#160+Game.Name
        else if Pos('[WWP]',Game.Name) = 1 then Delete(Game.Name,1,6);
        if Length(Game.Name) > 29 then Game.Name := Copy(Game.Name, 1, 29);
        if ClientVersion <> '2.0' then
        begin
          Game.Password:=Parameters.Values['Pass'];
          if (Game.Password <> '0') then Game.PassNeeded:='1'
          else Game.PassNeeded:='0';
        end
        else
        begin
          Game.Password:=Parameters.Values['Pwd'];
          if (Game.Password <> '') then Game.PassNeeded:='1'
          else Game.PassNeeded:='0';
        end;
        Game.LType:=Parameters.Values['Type'];
        Game.Chan:=DecodeURI(Parameters.Values['Chan']);
        Game.Loc:=Parameters.Values['Loc'];
        Game.HosterNickname:=DecodeURI(Parameters.Values['Nick']);
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
            if not (ClientVersion = '2.0') then
              Headers:=Headers+'SetGameId: : '+IntToStr(Game.GameID)+#13#10
            else
              Headers:=Headers+'SetGameId: '+IntToStr(Game.GameID)+#13#10;
            Body:='<html>'#10'<head><title>Object moved</title></head>'#10'<body>'#10'<h1>Object moved</h1>'#10'This object may be found <a href="/wormageddonweb/GameList.asp?Channel='+Game.Chan+'">here</a>.'#10'</body>'#10'</html>';
            // The string above is for compatibility with Wheat Snooper, otherwise it can't host: Yes, I know, it's quite stupid.
          end
          else
          begin
            EventLog(Format(L_GAME_FAIL_NONGAMING, [Game.HosterNickname, Game.Name, Game.HosterAddress, Channel.Name, Game.HostedFrom]));
            Body:='<NOTHING>'#10;
          end
        else
          begin
          EventLog(Format(L_GAME_FAIL_NONEXISTENT_CHAN, [Game.HosterNickname, Game.Name, Game.HosterAddress, Game.Chan, Game.HostedFrom]));
          Body:='<NOTHING>'#10;
          end;
        end
      else
      if (TextMatch(CmdParam,'Close')) or (TextMatch(CmdParam,'Delete')) then
      begin
        N:=-1;
        IParam:=Parameters.Values['GameID'];
        if Pos(': ',IParam) = 1 then
          Delete(IParam, 1, 2);
        for I:=0 to Length(Games)-1 do
          if IntToStr(Games[I].GameID)=IParam then
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
              EventLog(Format(L_GAME_FAIL_SABOTAGE, [ConnectingFrom, Games[I].HosterNickname, IParam, Games[I].HostedFrom]));
              Break;
            end;
        if N=-1 then
          begin
          //raise Exception.Create('No such game');
          //EventLog(Format(L_GAME_FAIL_NONEXISTENT_GAME, [ConnectingFrom, Parameters.Values['GameID']]));
          end
        else
          EventLog(Format(L_GAME_CLOSED_GRACEFULLY, [Game.HosterNickname, Game.Name]));
      end
      else
        Body:='<NOTHING>'#10
    end
    else
    if AspPhp(FileName,'GameList')then
      begin
      CleanUpGames;
      Body:=Body+'<GAMELISTSTART>'#10;
      if not (ClientVersion = '2.0') then
      begin
        Str:=Parameters.Values['Channel'];
        for I:=0 to Length(Games)-1 do
          with Games[I] do
            if TextMatch(Str,Chan) then
              Body:=Body+'<GAME '+Name+' '+HosterNickname+' '+HosterAddress+' '+Loc+' 1 '+PassNeeded+' '+IntToStr(GameID)+' '+LType+'><BR>'#10;
      end
      else
        Body:=Body+'<GAME Y0vnIZImSP06P7O1HjKcOY2bK9+FV33pM1hJISZmSEV++smra8z+Fw2XfwhQ8p2sKA3'+'TaLX2CN6S0SO7rfmkBQ3afllCrgIeESvx6ugeEZr7DxoQz8Xtr/bm3V+/y66'+'MadQSp8oaodh5jp3EUDgYrHq8teeivBlriVZ8I8t0AyKUMPzjHa0xuoiTH1Q8e+v'+'EvlsXN/I5aUrk4THcVIclU4d0NTTjtuzWEURHV4+VRgoq0MdmVbE1wlan5ZksEPsHX/vZ/wweiNmL55M7Op25WL7tR0u'+'NVk9DSE8zmFO2kPVlPp/AKMoOJsvOGrpN2MBmdCNgFxTiDXo6f2i0OxCZpw==>'#10;
      Body:=Body+'<GAMELISTEND>'#10;
      end
    else
    if AspPhp(FileName,'Rankings')  then
      if VersionThisNewer(ClientVersion,'3.6.29.10') then
        Body:='<CENTER><FONT SIZE=4 COLOR="White">Sorry, this server doesn''t support rankings.</FONT></CENTER>'#10
      else
        Body:='<FONT Size=2 Colour=0>Sorry, this server doesn''t support rankings.</FONT>'#10
    else
    if AspPhp(FileName,'ProcessGameResult') then
      Body:='<NOTHING>'#10
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
      if (FileName='') or (FileName[Length(FileName)]=PathDelim) then
        FileName:=FileName+'index.html';
      if FileExists('wwwroot'+PathDelim+FileName) then
      begin
        Log('[HTTP] '+ConnectingFrom+' '+L_HTTP_FILE_SENDING+' '+FileName);
        S:='application/octet-stream';
        for I:=1 to High(MimeTypes) do
          if '.'+MimeTypes[I].Extension=ExtractFileExt(FileName) then
            S:=MimeTypes[I].MimeType;
        ExternalFile:=true;
        Headers:=Headers+'Content-Type: '+S+#13#10;
        Body:=GetFile('wwwroot'+PathDelim+FileName);
      end
      else
        raise Exception.Create('"File" not found - '+FileName);
      end;

    if not ExternalFile then
      Headers:=Headers+'Content-Type: text/html'#13#10;
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


procedure TRequest.SendLn(S: string; Logging: Boolean=true);
var
  AStr: AnsiString;
begin
  AStr:=AnsiString(S);
  AStr:=AStr+#13#10;
  if send(Socket, AStr[1], Length(AStr), 0)<>Length(AStr) then
    Log('[HTTP] > '+L_FAILED+' ('+WinSockErrorCodeStr(WSAGetLastError)+')');
procedure TRequest.ResumeThread;
begin
  {$IFDEF MSWINDOWS}
  {$IF CompilerVersion >= 21}
  Start;
  {$ELSE}
  Resume;
  {$IFEND}
  {$ELSE}
  Start;
  {$ENDIF}
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

function AspPhp(S, PageName: String): Boolean;
var
  I: Integer;
  OrigName: string;
begin
  Result:=false;
  OrigName:=S;
  if Copy(S, 1, 15)='wormageddonweb/' then
    Delete(S, 1, 15)
  else if Copy(S, 1, 7)='wwpweb/' then
    Delete(S, 1, 7)
  else if Copy(S, 1, 7)='wwpnet/' then
    Delete(S, 1, 7);
  while (Pos('/',OrigName) <> 0) and (PathDelim='\') do
    OrigName[Pos('/',OrigName)]:='\';
  if TextMatch(S, PageName+'.asp') or TextMatch (S, PageName+'.php') then
    if not ((AllowArbitrary) and FileExists('wwwroot'+PathDelim+OrigName)) then
      Result:=true;
end;

end.
