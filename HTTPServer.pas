unit HTTPServer;

{$IFDEF FPC}
{$mode DELPHI}
{$ENDIF}

interface
uses
  Classes, Types,
{$IFDEF MSWINDOWS}
  WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  IRCServer, Localization, Version;

type
  TRequest=class(TThread)
    private
      Socket: TSocket;
      White: Boolean;

    public
      ConnectingFrom: string;
      FileName: string;
      UserAgent: string;
      Parameters: TStringList;
      ClientVersion: TVersion;
      procedure ResumeThread;
      procedure Execute; override;
      procedure SendLn(S: string; Logging: Boolean=true);
    end;

  TGame=class(TObject)
    private
      function GetGameAge: Int64;

    public
      Created: TDateTime;
      Name, Password, Loc, Chan, LType: string;
      HosterNickname, HosterAddress, HostedFrom: string;
      PassNeeded: char;
      Scheme: string;
      GameID: Integer;
      property Age: Int64 read GetGameAge;
    end;

var
  GameThreadList: TThreadList;
  GameCounter: Integer=1000000;
  ThreadID: Cardinal = 0;

procedure StartHTTPServer;
procedure Freedom;
function AspPhp(S, PageName: string): Boolean;
function CustomAccept(s: TSocket; addr: PSockAddr; AddrLen: PInteger; Context: Pointer = nil): TSocket;

implementation
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Base, Lists, DateUtils, Data;

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
  Headers, Body: String;
  TargChanName, TargChanScheme: String;
  I, R, Bytes: Integer;
  ExternalFile: Boolean;
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
        Log('[HTTP] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').', false, HTTPtoConsole);
        Exit;
      end;
      if Bytes=0 then
      begin
        Sleep(10);
        Continue;
      end;
      SetLength(SA, Bytes);
      R:=recv(Socket, SA[1], Length(SA), 0);
      if(R=0)or(R=SOCKET_ERROR)then
      begin
        Log('[HTTP] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').', false, HTTPtoConsole);
        Exit;
      end;
      SetLength(SA, R);
      BufferA := BufferA + SA;
      Buffer := String(BufferA);
      S := String(SA);
    until Copy(BufferA, Length(BufferA)-3, 4)=#13#10#13#10;      // ends with an empty line

    GetLine(Buffer, S);
    Log('[HTTP] '+ConnectingFrom+' '+S, false, HTTPtoConsole);
    if Copy(S, 1, 4)<>'GET ' then
      raise Exception.Create('Only GET requests are supported');
    Delete(S, 1, 4);
    S:=StringSection(S, 0);
    S:=StrEncodePURLE(S);
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
    Parameters.NameValueSeparator:='=';
    while S<>'' do
    begin
      Parameters.Add(Copy(S, 1, Pos('&', S)-1));
      Delete(S, 1, Pos('&', S));
    end;

    while GetLine(Buffer,S) do
      if Pos('User-Agent: ',S) = 1 then
      begin
        Delete(S, 1, 12);
        UserAgent:=S;
        if Pos('T17Client',UserAgent) = 1 then
        begin
          Delete(S, 1, 10);
          if S <> '' then
            ClientVersion.Str:=StringSection(S, 0)
          else
            ClientVersion.Str:='1.0';
        end;
      end
      else if (Pos('Requester: ',S) = 1) and (ConnectingFrom = '127.0.0.1') then
        ConnectingFrom:=StringSection(S, 1);

    if ConnectingFrom='127.0.0.1' then
      Target:=ConnectingFrom
    else
      Target:=ServerHost;

    ExternalFile:=false;
    Headers:='HTTP/1.1 200 OK'#13#10;
    Headers:=Headers+'Server: MyWormNET/'+APPVERSION+#13#10;
    Headers:=Headers+'X-Powered-By: MyWormNET'#13#10;
    Body:='';
    if AspPhp(FileName,'Login') or AspPhp(FileName,'Connect')then
    begin
      if ClientVersion.Valid
       and (ClientVersion.Str <> '2.0')
       and ClientVersion.IsOlderThan(MinimumVersion)
       and (MinimumVersion.IsAtLeast('3.6.29.10')
       or not ClientVersion.IsBetween('1.0', '2.0')) then
        raise Exception.Create('Sorry, your version of the game is too old. Please update via http://wa.team17.com')
      else if ClientVersion.IsAtLeast('3.6.29.29') then
      begin
        if IRCPassword <> IRCDefPassword then
          Body:='<CONNECT '+Target+' IRCPASS='+IRCPassword+'>'#10
        else
          Body:='<CONNECT '+Target+'>'#10;
        Body:=Body+#13#10'<MOTD>'+GetTextFile('news.xml')+'</MOTD>'#10;
      end
      else
        if ClientVersion.Str = '2.0' then
          Body:='<CONNECT '+Target+' IRCPORT='+IntToStr(IRCPort)+' IRCUSER=WWP IRCPASS='+IRCPassword+'>'
        else if ClientVersion.Valid and (IRCPassword <> IRCDefPassword) then
          Body:='<CONNECT '+Target+' IRCPASS='+IRCPassword+'>'#10
        else
          Body:='<CONNECT '+Target+'>'#10;
    end
    else
    if AspPhp(FileName,'UpdatePlayerInfo') then
      if ClientVersion.Str = '2.0' then
        Body:='<CONNECT '+Target+' IRCPORT='+IntToStr(IRCPort)+' IRCUSER=WWP IRCPASS='+IRCPassword+'>'
      else if ClientVersion.Valid and (IRCPassword <> IRCDefPassword) then
        Body:='<CONNECT '+Target+' IRCPASS='+IRCPassword+'>'#10
      else
        Body:='<CONNECT '+Target+'>'#10
    else
    if AspPhp(FileName,'WelcomeLoginForm') then
    begin
      if ClientVersion.Str = '2.0' then
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
      if ClientVersion.IsAtLeast('3.6.29.10') then
      begin
        Body:=Body+'<CENTER><FONT SIZE=3 COLOR="White">Welcome to '+NetworkName+'</FONT></CENTER><BR><BR><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=2 COLOR=0>You can use any username / password to log in</FONT></CENTER><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=1 COLOR="Silver">Your IP address is detected as: '+ConnectingFrom+'</FONT></CENTER><BR><BR>'#10;
        Body:=Body+'<CENTER><FONT SIZE=2><A HREF="/wormageddonweb/LoginForm.asp">Login</A></FONT></CENTER>'#10;
      end
      else
        Body:='<FONT SIZE=2><A HREF="/wormageddonweb/LoginForm.asp">Login</A></FONT>'#10;
    end
    else
    if AspPhp(FileName,'SelectServer') or AspPhp(FileName,'LoginForm') then
      Body:='<SHOWLOGIN>'#10
    else
    if AspPhp(FileName,'RequestAuth') then
      Body:='<ANSWER LIES+HERE+FOR+YOU+MY+FRIEND>'#10
    else
    if AspPhp(FileName,'RequestChannelScheme') then
    begin
      Str:='#'+Parameters.Values['Channel'];
      Channel:=LockChannelByName(Str);
      if Channel <> nil then
        Body:='<SCHEME='+Channel.Scheme+'>'#10
      else
        Body:='<SCHEME=Pf,Be>'#10;
      ChannelThreadList.UnlockList;
    end
    else
    // Cmd=Create&Name=ßCyberShadow-MD&HostIP=cybershadow.no-ip.org&Nick=CyberShadow-MD&Pwd=123&Chan=AnythingGoes&Loc=40&Type=0 HTTP/1.0
    if AspPhp(FileName,'Game') then
    begin
      CmdParam:=Parameters.Values['Cmd'];
      if TextMatch(CmdParam,'Create') then
      begin
        if not White then
        begin
          GameList:=GameThreadList.LockList;
          for I:=0 to GameList.Count-1 do with TGame(GameList[I]) do
            if HostedFrom = ConnectingFrom then
            begin
              EventLog(Format(L_GAME_CLOSED_ANTIFLOOD, [HosterNickname, Name, ConnectingFrom]));
              GameList.Remove(GameList[I]);
              Break;
            end;
          GameThreadList.UnlockList;
        end;

        Str:='#'+StrDecodePURLE(Parameters.Values['Chan']);
        Channel:=LockChannelByName(Str);
        if Channel <> nil then
        begin
          TargChanName := Channel.Name;
          TargChanScheme := Channel.Scheme;
        end;
        ChannelThreadList.UnlockList;
        if Channel <> nil then
          if Pos('Tf',TargChanScheme) = 0 then
          begin
            Game:=TGame.Create;
            with Game do
            begin
              Name:=StrDecodePURLE(Parameters.Values['Name']);
              if ClientVersion.Str = '2.0' then Name:='[WWP]'#160+Name
              else if Pos('[WWP]', Name) = 1 then Delete(Name,1,6);
              if Length(Name) > 29 then Name := Copy(Name, 1, 29);
              if ClientVersion.Str = '2.0' then
              begin
                Password:=Parameters.Values['Pass'];
                if (Password <> '0') then PassNeeded:='1'
                else PassNeeded:='0';
              end
              else
              begin
                Password:=Parameters.Values['Pwd'];
                if (Password <> '') then PassNeeded:='1'
                else PassNeeded:='0';
              end;
              LType:=Parameters.Values['Type'];
              Chan:=StrDecodePURLE(Parameters.Values['Chan']);
              Loc:=Parameters.Values['Loc'];
              HosterNickname:=StrDecodePURLE(Parameters.Values['Nick']);
              HosterAddress:=Parameters.Values['HostIP'];
              HostedFrom:=ConnectingFrom;
              Scheme:=TargChanScheme;
              GameID:=InterLockedIncrement(GameCounter);
              Created:=Now;
              EventLog(Format(L_GAME_CREATED_INFO, [HosterNickname, HostedFrom, Name, '#'+Chan, MakeWALink(HosterAddress, IntToStr(GameID), Scheme)]));
              GameThreadList.Add(Game);

              if ClientVersion.Str <> '2.0' then
                Headers:=Headers+'SetGameId: : '+IntToStr(GameID)+#13#10
              else
                Headers:=Headers+'SetGameId: '+IntToStr(GameID)+#13#10;
              Body:='<html>'#10'<head><title>Object moved</title></head>'#10'<body>'#10'<h1>Object moved</h1>'#10'This object may be found <a href="/wormageddonweb/GameList.asp?Channel='+Chan+'">here</a>.'#10'</body>'#10'</html>';
              // The string above is for compatibility with Wheat Snooper 2.8-, otherwise it can't host: Yes, I know, it's quite stupid.
            end;
          end
          else
          begin
            EventLog(Format(L_GAME_FAIL_NONGAMING, [StrDecodePURLE(Parameters.Values['Nick']), ConnectingFrom, StrDecodePURLE(Parameters.Values['Name']), Parameters.Values['HostIP'], TargChanName, ConnectingFrom]));
            Body:='<NOTHING>'#10;
          end
        else
        begin
          EventLog(Format(L_GAME_FAIL_NONEXISTENT_CHAN, [StrDecodePURLE(Parameters.Values['Nick']), ConnectingFrom, StrDecodePURLE(Parameters.Values['Name']), Parameters.Values['HostIP'], TargChanName, ConnectingFrom]));
          Body:='<NOTHING>'#10;
        end;
      end
      else
      if (TextMatch(CmdParam,'Close')) or (TextMatch(CmdParam,'Delete')) then
      begin
        IParam:=Parameters.Values['GameID'];
        if Pos(': ',IParam) = 1 then
          Delete(IParam, 1, 2);
        GameList:=GameThreadList.LockList;
        for I:=0 to GameList.Count-1 do with TGame(GameList[I]) do
        begin
          if IntToStr(GameID)=IParam then
            if (ConnectingFrom = HostedFrom) or (ConnectingFrom = '127.0.0.1') then
            begin
              EventLog(Format(L_GAME_CLOSED_GRACEFULLY, [HosterNickname, Name]));
              GameList.Remove(GameList[I]);
              Free;
              Break;
            end
            else
            begin
              EventLog(Format(L_GAME_FAIL_SABOTAGE, [ConnectingFrom, HosterNickname, IParam, HostedFrom]));
              Break;
            end;
        end;
        GameThreadList.UnlockList;
      end
      else
        Body:='<NOTHING>'#10
    end
    else
    if AspPhp(FileName,'GameList')then
      begin
      CleanUpGames;
      Body:=Body+'<GAMELISTSTART>'#10;
      if not (ClientVersion.Str = '2.0') then
      begin
        Str:=Parameters.Values['Channel'];
        GameList:=GameThreadList.LockList;
        for I:=0 to GameList.Count-1 do with TGame(GameList[I]) do
          if TextMatch(Str, Chan) then
            Body:=Body+'<GAME '+Name+' '+HosterNickname+' '+HosterAddress+' '+Loc+' 1 '+PassNeeded+' '+IntToStr(GameID)+' '+LType+'><BR>'#10;
        GameThreadList.UnlockList;
      end
      else
        Body:=Body+'<GAME Y0vnIZImSP06P7O1HjKcOY2bK9+FV33pM1hJISZmSEV++smra8z+Fw2XfwhQ8p2sKA3'+'TaLX2CN6S0SO7rfmkBQ3afllCrgIeESvx6ugeEZr7DxoQz8Xtr/bm3V+/y66'+'MadQSp8oaodh5jp3EUDgYrHq8teeivBlriVZ8I8t0AyKUMPzjHa0xuoiTH1Q8e+v'+'EvlsXN/I5aUrk4THcVIclU4d0NTTjtuzWEURHV4+VRgoq0MdmVbE1wlan5ZksEPsHX/vZ/wweiNmL55M7Op25WL7tR0u'+'NVk9DSE8zmFO2kPVlPp/AKMoOJsvOGrpN2MBmdCNgFxTiDXo6f2i0OxCZpw==>'#10;
      Body:=Body+'<GAMELISTEND>'#10;
      end
    else
    if AspPhp(FileName,'Rankings')  then
      if ClientVersion.IsAtLeast('3.6.29.10') then
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
          FileName[I]:=Chr(StrToIntDef('$'+Copy(FileName, I+1, 2), Ord('%')));
          Delete(FileName, I+1, 2);
        end;
      if Pos('..', FileName)+Pos(PathDelim+PathDelim, FileName)<>0 then
        raise Exception.Create('hmm hmm hmm');
      if (FileName='') or (FileName[Length(FileName)]=PathDelim) then
        FileName:=FileName+'index.html';
      if FileExists('wwwroot'+PathDelim+FileName) then
      begin
        Log('[HTTP] '+ConnectingFrom+' '+L_HTTP_FILE_SENDING+' '+FileName, false, HTTPtoConsole);
        S:='application/octet-stream';
        for I:=1 to High(MimeTypes) do
          if '.'+MimeTypes[I].Extension=ExtractFileExt(FileName) then
            S:=MimeTypes[I].MimeType;
        ExternalFile:=true;
        Headers:=Headers+'Content-Type: '+S+#13#10;
        Body:=GetBinaryFileAsString('wwwroot'+PathDelim+FileName);
      end
      else
        raise Exception.Create('"File" not found - '+FileName);
      end;

    if not ExternalFile then
      Headers:=Headers+'Content-Type: text/html'#13#10;
    Headers:=Headers+'Content-Length: '+IntToStr(Length(Body))+#13#10;
    S:=Headers+#13#10+Body;
    SendLn(S);
  except
    on E: Exception do
      try
        Log('[HTTP] '+L_ERROR_WITH+' '+ConnectingFrom+' : '+E.Message, false, HTTPtoConsole);
        S:=Headers+'Error: : '+E.Message+#13#10#13#10+'Error: '+E.Message;
        SendLn(S);
      except
        end;
    end;
  Parameters.Free;
  ClientVersion.Free;
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
    Log('[HTTP] > '+L_ERROR_FAILED+' ('+WinSockErrorCodeStr(WSAGetLastError)+')', false, HTTPtoConsole);
end;

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

function TGame.GetGameAge: Int64;
begin
  Result:=SecondsBetween(Now, Created);
end;

// ***************************************************************

procedure Freedom;
var
  I: Integer;
  GameList: TList;
begin
  GameList:=GameThreadList.LockList;
  for I := GameList.Count-1 downto 0 do
  begin
    TGame(GameList[I]).Free;
    GameList.Remove(GameList[I]);
  end;
  GameThreadList.UnlockList;
end;

function CustomAccept(s: TSocket; addr: PSockAddr; AddrLen: PInteger; Context: Pointer): TSocket;
var
  TargetIP: string;
begin
  Result:=accept(s, addr, AddrLen);
  if Result <> INVALID_SOCKET then
  begin
    TargetIP:=String(inet_ntoa(addr.sin_addr));
    if InList(IPBanThreadList, TargetIP) then
    begin
      Log(Format(L_CONNECTION_REJECTED, [TargetIP, Word(Context^)]));
      closesocket(Result);
      Result:=0;
    end;
  end;
end;

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  ServicePort: Integer;
  T: Integer;
  Request: TRequest;
begin
  Result:=0;
  GameThreadList.Clear;

  ServicePort:=HTTPPort;

  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr( '0.0.0.0' );
  service.sin_port := htons( ServicePort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
  begin
    EventLog('[HTTP] '+Format(L_ERROR_BIND, [ServicePort, WinSockErrorCodeStr(WSAGetLastError)]));
    Exit;
  end;
  if listen( m_socket, 50 )=SOCKET_ERROR then
  begin
    EventLog('[HTTP] '+Format(L_ERROR_BIND, [ServicePort, WinSockErrorCodeStr(WSAGetLastError)]));
    Exit;
  end;
  EventLog('[HTTP] '+Format(L_SERVICE_LISTENING, [ServicePort]));

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := CustomAccept(m_socket, @incoming, @T, @HTTPPort);
    if AcceptSocket=INVALID_SOCKET then
      Sleep(5)
    else if AcceptSocket<>0 then
    begin
      T:=SizeOf(incoming);
      Log('[HTTP] '+Format(L_CONNECTION_ESTABLISHED, [inet_ntoa(incoming.sin_addr)]), false, HTTPtoConsole);

      Request:=TRequest.Create(true);
      Request.Socket:=AcceptSocket;
      Request.ConnectingFrom:=String(inet_ntoa(incoming.sin_addr));
      Request.White:=false;
      if InList(WhiteIPThreadList, Request.ConnectingFrom) then
        Request.White:=true;
      Request.UserAgent:='Unknown';
      Request.Parameters:=TStringList.Create;
      Request.ClientVersion:=TVersion.Create;
      Request.ResumeThread;
    end;
  until False;
end;

procedure StartHTTPServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

function AspPhp(S, PageName: String): Boolean;
var
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

initialization
  GameThreadList:=TThreadList.Create;

finalization
  GameThreadList.Free;

end.

