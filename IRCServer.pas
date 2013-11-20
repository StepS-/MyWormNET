unit IRCServer;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface
uses                                                            
{$IFDEF WIN32}
  Windows, WinSock,
{$ELSE}
  Sockets, FakeWinSock,
{$ENDIF}
  Classes;

type
  TUser=class (TThread)
    ConnectingFrom: string;
    Nickname, Username, Hostname, Servername, Realname: string;
    LastSenior: string;
    LastBanTime: Int64;
    Socket: TSocket;
    InChannel: array of Boolean;
    Modes: array[char] of Boolean;

    procedure Execute; override;
    procedure SendLn(S: string);
    procedure LogIn(S: String);
    procedure SendError(S: String; ErrNo: Integer);

    procedure ExecNick(S: String);
    procedure ExecUser(S: String);
    procedure ExecIplookup(S: String);
    procedure ExecPrank(S: String);
    procedure ExecKickall(S: String);
    procedure ExecSendraw(S: String);
    procedure ExecAnnounce(S: String);
    procedure ExecIson(S: String);
    procedure ExecQuit(S: String);
    procedure ExecJoin(S: String);                                                                                         
    procedure ExecNames(S: String);
    procedure ExecPart(S: String);
    procedure ExecMode(S: String);
    procedure ExecWho(S: String);
    procedure ExecList(S: String);
    procedure ExecExpect(S: String);
    procedure ExecGames(S: String);
    procedure ExecKill(S: String);
    procedure ExecOper(Command, S: String);
    procedure ExecMessage(Command, S: String);
    procedure ExecMute(Mute: Boolean; S: String);

    function Registered: Boolean;
    function ChangeMode(Side, Mode: Char; Master: String): Boolean;
    end;

  TChannel=class (TObject)
    Name, Scheme, Topic: String;
    Number: Integer;

    constructor Create(Name, Scheme, Topic: String);
    destructor Destroy; override;

    end;

var
  Users: array of TUser;
  Channels: array of TChannel;
  LastStr: string;

procedure StartIRCServer;
procedure GetChannels;
procedure LogToOper(S: string);
function AddChannel(Name, Scheme, Topic: String): Boolean;
function ChannelByName(Name: String): TChannel;
function NickInUse(Nick: string): Boolean;

resourcestring
  IRCPrefModes='qaohv';
  IRCPrefixes='~&@%+';
  IRCPassword='ELSILRACLIHP ';
  IRCPassword2='ELSILRACLIHP';
  ValidNickChars='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`-';

implementation
uses
  Base, Data, SysUtils, IniFiles, HTTPServer, WormNATServer;

procedure TUser.Execute;
var
  Buffer, S, Command: string;
  R, Bytes, I, J, N: Integer;
  PingTimer: Integer;
  ReadSet, ErrorSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
  Password: string;

begin
  try
    Buffer:='';
    PingTimer:=0;
    Password:='';
    repeat
      repeat
        ReadSet.count:=1;
        ReadSet.Socket:=Socket;
        ErrorSet.count:=1;
        ErrorSet.Socket:=Socket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;
        R:=select(Socket+1, @ReadSet, nil, @ErrorSet, @TimeVal);
        if (R=SOCKET_ERROR) or (ErrorSet.count>0) then
          begin
          Log('[IRC] '+ConnectingFrom+' select() error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error.');
          end;

        if (ReadSet.count=0)or(R=0) then
          Break;  // nothing to read

        R:=ioctlsocket(Socket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          end;

        if Bytes=0 then  // software disconnect
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error (Graceful disconnect).');
          raise Exception.Create('Software disconnect');
          end;

        SetLength(S, Bytes);
        R:=recv(Socket, S[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+')');
          end;
        SetLength(S, R);
        Buffer := Buffer + S;
        PingTimer:=0;
      until False;

      while GetLine(Buffer, S) do
      begin
        if Registered then
          Log('[IRC] < :'+Nickname+'!'+Username+'@'+StealthIP+' '+S)
        else
          Log('[IRC] < :'+ConnectingFrom+' '+S);

        Command:=UpperCase(Copy(S, 1, Pos(' ', S+' ')-1));
        Delete(S, 1, Length(Command)+1);
          
          if Command='NICK' then
            ExecNick(S)
        else
          if Command='USER' then
            ExecUser(S)
        else
          if Command='PASS' then
            Password:=S
        else
          if Command='PING' then
            SendLn('PONG :'+ServerHost)
        else
          if Command='PONG' then
        else
          if Registered then
            if Command='IPLOOKUP' then
              ExecIplookup(S)
          else
            if Command='PRANK' then
              ExecPrank(S)
          else
            if Command='KICKALL' then
              ExecKickall(S)
          else
            if Command='SENDRAW' then
              ExecSendraw(S)
          else
            if Command='ANNOUNCE' then
              ExecAnnounce(S)
          else
            if Command='ISON' then
              ExecIson(S)
          else
            if Command='QUIT' then
              ExecQuit(S)
          else
            if Command='JOIN' then
              ExecJoin(S)
          else
            if Command='NAMES' then
              ExecNames(S)
          else
            if Command='PART' then
              ExecPart(S)
          else
            if Command='MODE' then
              ExecMode(S)
          else
            if Command='WHO' then
              ExecWho(S)
          else
            if Command='LIST' then
              ExecList(S)
          else
            if Command='EXPECT' then
              ExecExpect(S)
          else
            if Command='GAMES' then
              ExecGames(S)
          else
            if (Command='KICK') or (Command='KILL') then
              ExecKill(S)
          else
            if (Command='OPER') or (Command='TAKEOWN') then
              ExecOper(Command,S)
          else
            if (Command='PRIVMSG') or (Command='NOTICE') then
              ExecMessage(Command,S)
          else
            if Command='MUTE' then
              ExecMute(true,S)
          else
            if Command='UNMUTE' then
              ExecMute(false,S)
          else
            if Command='TIME' then
              SendLn(':'+ServerHost+' 391 '+Nickname+' '+ServerHost+' :'+TextDateTimeNow)
          else
            if Command='AWAY' then
          else
            if Command='WHOIS' then
          else
            SendLn(':'+ServerHost+' 421 '+Nickname+' '+Command+' :Unknown command')
        else
          if (Command<>'') then
            SendError(Command,451);
      end;

      Inc(PingTimer);
      if PingTimer=18000 then
        SendLn('PING :'+ServerHost);
      if PingTimer=24000 then
      begin
        if not Modes['i'] then
          for I:=0 to Length(Channels)-1 do
            if InChannel[Channels[I].Number] then
              for J:=0 to Length(Users)-1 do
                if Users[J].InChannel[Channels[I].Number] then
                  Users[J].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :Ping timeout');
        if (Socket <> 0) then closesocket(Socket); Socket:=0;
        Break;
      end;
    until Socket=0;
    Log('[IRC] Closing link to '+ConnectingFrom);
    closesocket(Socket);

  except
    on E: Exception do
      begin
        if not Modes['i'] then
          for I:=0 to Length(Channels)-1 do
            if InChannel[Channels[I].Number] then
              for J:=0 to Length(Users)-1 do
                if Users[J].InChannel[Channels[I].Number] then
                try
                  Users[J].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+E.Message);
                except
                end;
      Log('[IRC] Error with '+ConnectingFrom+' : '+E.Message);
      end;
    end;

  if Socket<>0 then
    closesocket(Socket);   // ignore errors
  Socket:=0;

  EventLog(Nickname+' ('+ConnectingFrom+') has disconnected.');

  // TODO: add some sync lock or something here
  N:=-1;
  for I:=0 to Length(Users)-1 do
    if Users[I]=Self then
      N:=I;
  if N=-1 then
    Log(ConnectingFrom+': WTF can''t find myself!')
  else
    begin
    for I:=N to Length(Users)-2 do
      Users[I]:=Users[I+1];
    SetLength(Users, Length(Users)-1);
    end;
  FreeOnTerminate:=True;
end;

  
procedure TUser.LogIn(S: String);
var
  I, N, M: Integer;
  S2: String;
begin
  EventLog(Nickname+' ('+ConnectingFrom+') logged in.');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :Welcome, '+Nickname+'!');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :This is a custom WormNet IRC server emulator,');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :supporting only the base set of IRC features.');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :The server software was written by ');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :The_CyberShadow <thecybershadow@gmail.com>');
  SendLn(':'+ServerHost+' 011 '+Nickname+' :and extended by StepS <github.com/StepS->');
  SendLn(':'+ServerHost+' 003 '+Nickname+' :This server was created '+StartupTime);
  SendLn(':'+ServerHost+' 005 '+Nickname+' WALLCHOPS PREFIX=('+IRCPrefModes+')'+IRCPrefixes+' STATUSMSG='+IRCPrefixes+' CHANTYPES=# MAXCHANNELS=20 MAXBANS=25 NICKLEN=15 TOPICLEN=120 KICKLEN=90 NETWORK='+NetworkName+' CHANMODES=b,k,l,imnpstr MODES=6 :are supported by this server');
  if WormNATPort>0 then
    SendLn(':'+ServerHost+' 011 '+Nickname+' :[WormNATRouteOn:'+IntToStr(WormNATPort)+'] This server supports built-in WormNAT routing.');
  //SendLn(':'+ServerHost+' 007 '+Nickname+' :[YourIP:'+ConnectingFrom+'] Your external IP address is '+ConnectingFrom+'.');
  //SendLn(':'+ServerHost+' 004 '+Nickname+' wormnet1.team17.com 2.8/hybrid-6.3.1 oOiwszcrkfydnxb biklmnopstve');
  SendLn(':'+ServerHost+' 003 '+Nickname+' :Your host is '+ServerHost+', running MyWormNET version '+APPVERSION);
  N:=0;
  M:=0;
  for I:=0 to Length(Users)-1 do
    begin
    if (Users[I].Modes['q'])or(Users[I].Modes['a'])or(Users[I].Modes['o'])or(Users[I].Modes['h']) then
      Inc(N);
    if Users[I].Modes['i'] then
      Inc(M);
    end;
  SendLn(':'+ServerHost+' 251 '+Nickname+' :There are '+IntToStr(Length(Users)-M)+' users and '+IntToStr(M)+' invisible on this server.');
  SendLn(':'+ServerHost+' 252 '+Nickname+' '+IntToStr(N)+' :IRC Operators online');
  SendLn(':'+ServerHost+' 254 '+Nickname+' '+IntToStr(Length(Channels))+' :channels on this server');
  SendLn(':'+ServerHost+' 375 '+Nickname+' :- '+NetworkName+' Message of the Day - ');
  S:=GetFile('motd.txt')+#13#10;
  while GetLine(S, S2) do
   if(S<>'')or(S2<>'') then
    SendLn(':'+ServerHost+' 372 '+Nickname+' :- '+S2);
  SendLn(':'+ServerHost+' 376 '+Nickname+' :End of /MOTD command.');
end;

procedure TUser.SendError(S: String; ErrNo: Integer);
var StrOut: String;
begin
  case ErrNo of
  401:
    StrOut:='No such nick/channel';
  403:
    StrOut:='No such channel';
  404:
    StrOut:='Cannot send to channel';
  412:
    StrOut:='No text to send';
  432:
    StrOut:='Erroneous nickname';
  433:
    StrOut:='Nickname is already in use';
  451:
    StrOut:='Register first';
  461:
    StrOut:='Insufficient parameters';
  462:
    StrOut:='You may not reregister';
  464:
    StrOut:='Incorrect password';
  481:
    StrOut:='Insufficient privileges to execute this command';
  482:
    StrOut:='Insufficient privileges to change the requested mode for a given user';
  484:
    StrOut:='Insufficient privileges to act upon this user';
  502:
    StrOut:='The requested mode can only be applied to oneself';
  else
    StrOut:='Unknown error';
  end;
  
  SendLn(':'+ServerHost+' '+IntToStr(ErrNo)+' '+Nickname+' '+S+' :'+StrOut);
end;

procedure TUser.ExecIpLookup(S: String);
const Command='IPLOOKUP';
var I: Integer;
begin
  if S <> '' then
  begin
    if (Modes['q'])or(Modes['a'])or(Modes['o']) then
    begin
      for I:=0 to Length(Users)-1 do
      if S=Users[I].Nickname then
      begin
        SendLn(':SERVER'#160'MESSAGE!root@'+ServerHost+' PRIVMSG '+Nickname+' :IP of user '+S+' is: '+Users[I].ConnectingFrom+'.');
        Break
      end
      else if I=Length(Users)-1 then
        SendError(S,401);
    end
    else
      SendError(Command,481);
  end
  else
    SendError(Command,461);
end;

procedure TUser.ExecMute(Mute: Boolean; S: String);
var
  I: Integer;
  Side: Char;
  Pre, Command: String;
begin
  if Mute then Command:='MUTE'
  else Command:='UNMUTE';
  if S <> '' then
  begin
    if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
    begin
      for I:=0 to Length(Users)-1 do
        if S=Users[I].Nickname then
        begin
          if (Modes['q'])
          or ((Modes['a']) and not (Users[I].Modes['q']))
          or ((Modes['o']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']))
          or ((Modes['h']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']) and not (Users[I].Modes['o']))
          then
          begin
            if Mute then Side:='+'
            else Side:='-';

            if not Users[I].ChangeMode(Side,'b',Nickname) then
            begin
              if Mute then Pre:='already'
              else Pre:='not';
              SendLn(':'+ServerHost+' 401 '+Nickname+' '+S+' :This user has '+Pre+' been muted');
              Break
            end;

            Break;
          end
          else
          begin
            SendError(S,484);
            Break
          end;
        end
        else if I=Length(Users)-1 then
          SendError(S,401);
    end
    else
      SendError(Command,481);
  end
  else
    SendError(Command,461);
end;

procedure TUser.ExecKill(S: String);
const Command='KILL';
var
  I,J: Integer;
  Reason, Target: String;
begin
  if S <> '' then
  begin
    for I:=0 to Length(Channels)-1 do
      if Pos(Channels[I].Name, S) <> 0 then
      begin
        Delete(S, 1, Pos(' ', S));
        Break;
      end;
    if Pos(' ', S) <> 0 then
      begin
        Target:=Copy(S, 1, Pos(' ', S)-1);
        Reason:=Copy(S, Pos(' ', S)+1, Length(S));
        if Copy(Reason, 1, 1) = ':' then Delete(Reason, 1, 1);
      end
    else
      begin
        Target:=Copy(S, 1, Length(S));
        Reason:='No reason specified';
      end;
    if (Modes['q'])or(Modes['a'])or(Modes['o']){or(Modes['h'])} then
     begin
      for I:=0 to Length(Users)-1 do
        if Target=Users[I].Nickname then
          begin
            if (Modes['q'])
            or ((Modes['a']) and not (Users[I].Modes['q']))
            or ((Modes['o']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']))
      //    or ((Modes['h']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']) and not (Users[I].Modes['o']))
            then
              begin
                Users[I].LastSenior:=Nickname;
                for J:=0 to Length(Channels)-1 do
                  if Users[I].InChannel[Channels[I].Number] then
                     Users[I].InChannel[Channels[I].Number] := False;
                if not Users[I].Modes['i'] then
                  for J:=0 to Length(Users)-1 do
                    Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' QUIT :Killed by '+Nickname+': '+Reason);
                Users[I].SendLn('ERROR :You have been kicked from the server by '+Nickname+': '+Reason);
                EventLog(Users[I].Nickname+' has been killed by '+Nickname+': '+Reason);
                if (Users[I].Socket <> 0) then closesocket(Users[I].Socket); Users[I].Socket:=0;
                Break
              end
            else
              begin
                SendError(Target,484);
                Break
              end;
          end
        else if I=Length(Users)-1 then
          SendError(Target,401);
     end
    else
      begin
      SendError(Command,481);
      end;
    //        begin
    //          if InChannel then InChannel := False;
    //          for I:=0 to Length(Users)-1 do
    //            Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :Kicked: Attempted to use godly powers');
    //          SendLn('ERROR :Nice try, '+Nickname+'.');
    //          closesocket(Socket); Socket:=0;
    //        end;
   end
     else
       SendError(Target,401);
end;

procedure TUser.ExecPrank(S: String);
const Command='PRANK';
var
  I: Integer;
  Description, Target: String;
begin
  if Pos(' ', S) <> 0 then
  begin
    Target:=Copy(S, 1, Pos(' ', S)-1);
    Description:=Copy(S, Pos(' ', S)+1, Length(S));
  end
  else
  begin
    Target:=Copy(S, 1, Length(S));
    Description:='Something bad happened.';
  end;

  if (Modes['q'])or(Modes['a'])or(Modes['o']) then
    for I:=0 to Length(Users)-1 do
      if Target=Users[I].Nickname then
      begin
        if (Modes['q'])
        or ((Modes['a']) and not (Users[I].Modes['q']))
        or ((Modes['o']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']))
        then
          begin
            Users[I].LastSenior:=Nickname;
            Users[I].SendLn('ERROR :'+Description);
            EventLog(Users[I].Nickname+' has been pranked by '+Nickname+': '+Description);
            Break
          end
        else
        begin
          SendError(Target,484);
          Break
        end;
      end
  else if I=Length(Users)-1 then
    SendError(Target,401);
end;

procedure TUser.ExecKickall(S: String);
const Command='KICKALL';
var
  I,J: Integer;
begin
  if Modes['q'] then
  begin
    for I:=0 to Length(Users)-1 do
      if Users[I].Modes['q'] = false then
      begin
        if not Users[I].Modes['i'] then
          for J:=0 to Length(Users)-1 do
            Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' QUIT :Massive killing started by '+Nickname);
        Users[I].LastSenior:=Nickname;
        Users[I].SendLn('ERROR :Massive killing started by '+Nickname);
        if (Users[I].Socket <> 0) then closesocket(Users[I].Socket); Users[I].Socket:=0;
      end;
  end
  else
    SendError(Command,481);
end;

procedure TUser.ExecSendraw(S: String);
const Command='SENDRAW';
var I: Integer;
begin
  if (Modes['q']) then
  begin
    if (S='') then
      SendError(Command,461)
    else
      for I:=0 to Length(Users)-1 do
        Users[I].SendLn(S);
  end
  else
    SendError(Command,481);
end;

procedure TUser.ExecAnnounce(S: String);
const Command='ANNOUNCE';
var I, J: Integer;
begin
  if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
  begin
    if S <> '' then
    begin
      for I:=0 to Length(Channels)-1 do
        for J:=0 to Length(Users)-1 do
          if Users[J].InChannel[I] then
            Users[J].SendLn(':SERVER'#160'ANNOUNCEMENT!root@'+ServerHost+' NOTICE '+Channels[I].Name+' :'+S)
    end
    else
      SendError(Command,412);
  end
  else
    SendError(Command,481);
end;

procedure TUser.ExecIson(S: String);
const Command='ISON';
var
  I: Integer;
  IsonBuff, Target: String;
begin
  if Nickname <> '' then
  begin
    IsonBuff:='';
    while Pos(' ',S) <> 0 do
    begin
      Target:=Copy(S, 1, Pos(' ', S)-1);
      Delete(S, 1, Pos(' ', S));
      for I:=0 to Length(Users)-1 do
        if Target=Users[I].Nickname then
          IsonBuff:=IsonBuff+Target+' ';
    end;
    if S='' then
      SendLn(':'+ServerHost+' 303 '+Nickname+' :'+IsonBuff)
    else
    begin
      Target:=S;
      for I:=0 to Length(Users)-1 do
        if Target=Users[I].Nickname then
          IsonBuff:=IsonBuff+Target;
      SendLn(':'+ServerHost+' 303 '+Nickname+' :'+IsonBuff);
    end;
  end;
end;

procedure TUser.ExecNick(S: String);
const Command='NICK';
var I: Integer;
begin
  if Nickname<>'' then
    SendLn(':'+ServerHost+' 400 '+Nickname+' '+S+' :Nick change isn''t supported.')
  else
  begin
    SetLength(InChannel,Length(Channels));
    for I:=Length(S) downto 1 do
      if Pos(S[I], ValidNickChars)=0 then
        Delete(S, I, 1);
    if S='' then
      SendError(S,432)
    else
    begin
      if NickInUse(S) then
      SendError(S,433)
      else
      begin
        Nickname:=S;
        if Username<>'' then
          LogIn(S);
      end;
    end;
  end;
end;

procedure TUser.ExecUser;
const Command='USER';
// USER Username hostname servername :40 0 RO
begin
  if (Username='') or (Modes['q']) then
  begin
    Username:=Copy(S, 1, Pos(' ', S)-1);
    Delete(S, 1, Pos(' ', S));
    Hostname:=Copy(S, 1, Pos(' ', S)-1);
    Delete(S, 1, Pos(' ', S));
    Servername:=Copy(S, 1, Pos(' ', S)-1);
    Delete(S, 1, Pos(':', S));
    Realname:=S;

    LastSenior:='SERVER';

    if Username='' then
      Username:='Username'; //Prevent the Username from being blank (i.e. Wheat Snooper)
    if Nickname<>'' then
      LogIn(S);
  end
  else
    SendError(Command,462);
end;

procedure TUser.ExecQuit(S: String);
const Command='QUIT';
var I, J: Integer;
begin
// :CyberShadow!cybershado@38F7DF98.502358C0.F6DD7E74.IP QUIT :Input/output error
  if not (Modes['i']) then
    for I:=0 to Length(Channels)-1 do
      if InChannel[Channels[I].Number] then
      begin
        for J:=0 to Length(Users)-1 do
          if Users[J].InChannel[Channels[I].Number] then
            Users[J].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+Copy(S, 2, 1000));
      end;
end;

procedure TUser.ExecNames(S: String);
const Command='NAMES';
var
  I, K, N: Integer;
  StrOut: String;
  Channel: TChannel;
begin
  S:=Copy(S,1,Pos(' ',S+' ')-1);
  Channel:=ChannelByName(S);
  if Channel <> nil then
  begin
    S:=Channel.Name;
    N:=Channel.Number;
    StrOut:=':'+ServerHost+' 353 '+Nickname+' = '+S+' :';
    for I:=0 to Length(Users)-1 do
      if (Users[I].InChannel[N]) and not (Users[I].Modes['i']) then
      begin
        for K:=1 to Length(IRCPrefModes) do
          if Users[I].Modes[IRCPrefModes[K]] then
            StrOut:=StrOut+IRCPrefixes[K];
        StrOut:=StrOut+Users[I].Nickname+' ';
      end;
    SendLn(StrOut);
  end;
  SendLn(':'+ServerHost+' 366 '+Nickname+' '+S+' :End of /NAMES list.');
end;

procedure TUser.ExecJoin(S: String);
const Command='JOIN';
var
  I, K, N: Integer;
  CurChan: String;
  Channel: TChannel;
begin
  S:=S+',';
  repeat
  begin
    CurChan:=Copy(S,1,Pos(',',S)-1);
    Delete(S,1,Pos(',',S));
    Channel:=ChannelByName(CurChan);
//    EventLog(Nickname+' is attempting to join a channel '+CurChan);
    if Channel <> nil then
    begin
      CurChan:=Channel.Name;
      N:=Channel.Number;
      EventLog(Nickname+' has joined '+CurChan+'.');
      InChannel[N]:=True;
      //:CyberShadow-MD!Username@no.address.for.you JOIN :#AnythingGoes
      if not Modes['i'] then
        for I:=0 to Length(Users)-1 do
        begin
          if Users[I].InChannel[N] then
          begin
            Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN :'+CurChan);
            for K:=1 to Length(IRCPrefModes) do
              if Modes[IRCPrefModes[K]] then
                Users[I].SendLn(':'+ServerHost+' MODE '+CurChan+' +'+IRCPrefModes[K]+' '+Nickname);
          end;
        end
      else
        SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN :'+CurChan);
      ExecNames(CurChan);
    end
    else
      SendError(CurChan,403);
  end
  until Pos(',',S) = 0;
end;

procedure TUser.ExecPart(S: String);
const Command='PART';
var
   I, N: Integer;
   Channel: TChannel;
begin
    S:=Copy(S,1,Pos(' ',S+' ')-1);
    Channel:=ChannelByName(S);
    if Channel <> nil then
    begin
      S:=Channel.Name;
      N:=Channel.Number;
      EventLog(Nickname+' has left '+S);
      if Modes['i'] then
        SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+S)
      else
        for I:=0 to Length(Users)-1 do
          if Users[I].InChannel[N] then
            Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+S);
      InChannel[N]:=False;
    end;
end;

procedure TUser.ExecMode(S: String);
const Command='MODE';
var
  I, K: Integer;
  Side, Mode: Char;
  ModeStr, Target: String;
  Channel: TChannel;
begin
    Target:=Copy(S, 1, Pos(' ', S+' ')-1);
    ModeStr:='';
//    Delete(S, 1, Pos(':', S+':')-1);
//    if S<>'' then
//      SendLn(':'+ServerHost+' 472 '+Nickname+' :Sorry, you can''t set modes for anything.')
//    else
    Channel:=ChannelByName(Target);
      if Channel <> nil then
        begin
        Delete(S,1,Pos(Channel.Name,S));
        if Pos(' ', S) <> 0 then
          begin
          Delete(S,1,Pos(' ',S));
          if (Pos('+',S))or(Pos('-',S)) = 1 then
            begin
            if Pos(' ', S) <> 0 then
              begin
              if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
                begin
                ModeStr:=Copy(S, 1, Pos(' ', S));
                Delete(S, 1, Pos(' ', S));
                Target:=S;
                for I:=0 to Length(Users)-1 do
                  begin
                  if Users[I].Nickname=Target then
                    begin
                    for K:=2 to Pos(' ',ModeStr)-1 do
                      begin
                      Mode:=ModeStr[K];
                      Side:=ModeStr[1];
                      if (S<>' ') then
                        begin
                          if not
                            (
                              (((Mode='a') or (Mode='q')) and not (Modes['q']))
                              or (not (Mode='v') and not (Mode='b') and ((Modes['h'])
                                  and not (Modes['o']) and not (Modes['a']) and not (Modes['q'])))
                              or ((Mode='L') and (Target<>Nickname))
                              or ((Users[I].Modes['q']) and not (Modes['q']))
                              or ((Users[I].Modes['a']) and not (Modes['q']))
                              or ((Users[I].Modes['o']) and not (Modes['o']) and not (Modes['a']) and not (Modes['q']))
                            )
                          then
                            Users[I].ChangeMode(Side,Mode,Nickname)
                          else
                            if ((Mode='L') and (Target<>Nickname)) then
                              SendError(Command,502)
                            else
                            begin
                              SendError(Command,482);
                              Break;
                            end;
                        end;
                      end;
                    Break
                    end
                    else if I = Length(Users)-1 then
                      SendError(Target,401);
                  end;
                end
                else
                  SendError(Command,481);
              end
              else
                if Pos('+b',S) <> 0 then
                begin
                  for I:=0 to Length(Users)-1 do
                    if Users[I].Modes['b'] then
                      SendLn(':'+ServerHost+' 367 '+Nickname+' '+Channel.Name+' '+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' '+Users[I].LastSenior+' '+IntToStr(Users[I].LastBanTime));
                SendLn(':'+ServerHost+' 368 '+Nickname+' '+Channel.Name+' :End of Channel Ban List');
                end;
            end
            else
              SendLn(':'+ServerHost+' 324 '+Nickname+' '+Channel.Name+' +tn');
          end
          else
            SendLn(':'+ServerHost+' 324 '+Nickname+' '+Channel.Name+' +tn');
        end
        else
          SendError(Target,403);
   {     else
          begin
          User:=nil;
          for I:=0 to Length(Users)-1 do
            if Users[I].Nickname=Target then
              User:=Users[I];
          if User=nil then
            SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :No such nick/channel.')
          else
            begin
            S:='';
            for C:=#0 to #255 do
              if Modes[C] then
                S:=S+C;
            SendLn(':'+ServerHost+' 324 '+Nickname+' '+Target+' +'+S);
            end;
          end;     }
end;

procedure TUser.ExecMessage(Command, S: String);
var
  I, K, N: Integer;
  Target: String;
  User: TUser;
  Channel: TChannel;
begin
  if Modes['b'] then
    SendLn(':SERVER'#160'MESSAGE!root@'+ServerHost+' PRIVMSG '+Nickname+' :Sorry, but you are muted by '+LastSenior+' and thus cannot talk.')
  else
  begin
    Target:=Copy(S, 1, Pos(' ', S+' ')-1);
    Delete(S, 1, Pos(':', S+':')-1);
    Channel:=ChannelByName(Target);
    if Channel <> nil then
    begin
      Target:=Channel.Name;
      N:=Channel.Number;
      EventLog('['+Target+'] <'+Nickname+'> '+Copy(S, 1, 1000));
      for I:=0 to Length(Users)-1 do
        if Users[I].InChannel[N] and (Users[I]<>Self)then
          Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' '+Command+' '+Target+' '+S);
    end
    else
    begin
      User:=nil;
      for K:=1 to Length(IRCPrefixes) do
        if Pos(IRCPrefixes[K],Target) <> 0 then
          Delete(Target,Pos(IRCPrefixes[K],Target),1);
      for I:=0 to Length(Users)-1 do
        if LowerCase(Users[I].Nickname)=LowerCase(Target) then
          User:=Users[I];
      if User=nil then
        SendError(Target,401)
      else
      begin
        Target := User.Nickname;
        EventLog('['+Command+'] <'+Nickname+'> -> <'+Target+'> '+Copy(S, 1, 1000));
        LogToOper('['+Command+'] <'+Nickname+'> -> <'+Target+'> '+Copy(S, 1, 1000));
        User.SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' '+Command+' '+Target+' '+S);
      end;
    end;
    Sleep(1000); // throttle
  end;
end;

procedure TUser.ExecOper(Command, S: String);
var
  I, J: Integer;
  Mode: Char;
  Description: String;
begin
  if Copy(S, 1, Pos(' ', S+' ')-1)<>IRCOperPassword then
    Delete(S, 1, Pos(' ', S+' '));  // ignore username
  if S=IRCOperPassword then
  begin
    if Command='OPER' then
    begin
      Description:='Operator';
      Mode:='o';
    end
    else
    begin
      Description:='Owner';
      Mode:='q';
    end;
    EventLog(Nickname+' has registered as an '+Description+'.');
    Modes[Mode]:=True;
    for I:=0 to Length(Channels)-1 do
      if InChannel[Channels[I].Number] then
        for J:=0 to Length(Users)-1 do
          if Users[J].InChannel[Channels[I].Number] then
            Users[J].SendLn(':'+ServerHost+' MODE '+Channels[I].Name+' +'+Mode+' '+Nickname);
  end
  else
    SendError(Command,464);
end;

procedure TUser.ExecWho(S: String);
const Command='WHO';
var
  I, K: Integer;
  Pref, Target, ChanStr: String;
  Channel: TChannel;
begin
  //:wormnet1.team17.com 352 DoctorWho #AnythingGoes Username no.address.for.you wormnet1.team17.com TiCPU H :0 TiCpu
  //:wormnet1.team17.com 315 DoctorWho * :End of /WHO list.
  
  Channel:=ChannelByName(S);
  for I:=0 to Length(Users)-1 do
  begin
    Pref:='';
    for K:=1 to Length(IRCPrefModes) do
      if Users[I].Modes[IRCPrefModes[K]] then
      begin
        Pref:=IRCPrefixes[K];
        Break;
      end;

    if S<>'' then
      Target:=S
    else
      Target:=Users[I].Nickname;

    if Channel = nil then
    begin

      for K:=1 to Length(IRCPrefixes) do
        if Pos(IRCPrefixes[K],S) <> 0 then
          Delete(S,Pos(IRCPrefixes[K],S),1);

      if (S<>'') and (S<>Users[I].Nickname) then continue;

      ChanStr:='*';

      for K:=0 to Length(Channels)-1 do
        if Users[I].InChannel[Channels[K].Number] then
        begin
          ChanStr:=Channels[K].Name;
          Break;
        end;

      if Users[I].Nickname <> Nickname then
      begin
        if not (Users[I].Modes['i']) then
          if Users[I].Nickname <> '' then
            SendLn(':'+ServerHost+' 352 '+Nickname+' '+ChanStr+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Target+' H'+Pref+' :0 '+Users[I].Realname);
      end
      else
        SendLn(':'+ServerHost+' 352 '+Nickname+' '+ChanStr+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Target+' H'+Pref+' :0 '+Realname);
      if (S<>'') and (S=Users[I].Nickname) then Break;
    end
    else if Users[I].InChannel[Channel.Number] then
      begin
      if (Pos(' o ',S+' ') <> 0) and not ((Users[I].Modes['h']) or (Users[I].Modes['o']) or (Users[I].Modes['a']) or (Users[I].Modes['q']))
        then continue
      else
      if not (Users[I].Modes['i']) then
        if Users[I].Nickname <> Nickname then
          SendLn(':'+ServerHost+' 352 '+Nickname+' '+Channel.Name+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Users[I].Nickname+' H'+Pref+' :0 '+Users[I].Realname)
        else
          SendLn(':'+ServerHost+' 352 '+Nickname+' '+Channel.Name+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Nickname+' H'+Pref+' :0 '+Realname);
      end;
  end;
  if S='' then
    SendLn(':'+ServerHost+' 315 '+Nickname+' * :End of /WHO list.')
  else
    SendLn(':'+ServerHost+' 315 '+Nickname+' '+S+' :End of /WHO list.');
end;

procedure TUser.ExecList(S: String);
const Command='LIST';
var I, J, N: Integer;
begin                     
  SendLn(':'+ServerHost+' 321 '+Nickname+' Channel :Users  Name');
  for I:=0 to Length(Channels)-1 do
  begin
    N:=0;
    for J:=0 to Length(Users)-1 do
      if Users[J].InChannel[Channels[I].Number] then
        Inc(N);
    SendLn(':'+ServerHost+' 322 '+Nickname+' '+Channels[I].Name+' '+IntToStr(N)+' :'+Channels[I].Topic);
  end;
  SendLn(':'+ServerHost+' 323 '+Nickname+' :End of /LIST');
end;

procedure TUser.ExecExpect(S: String);
const Command='EXPECT';
var
  I: Integer;
  User: TUser;
begin
  Log('Received EXPECT command from '+ConnectingFrom+' for '+S);
  User:=nil;
  for I:=0 to Length(Users)-1 do
  if Users[I].Nickname=S then
    User:=Users[I];
    if User=nil then
      SendError(S,401)
    else
    begin
      SendLn(':'+ServerHost+' NOTICE '+Nickname+' :OK, expecting '+User.Nickname+' from '+StealthIP);
      PrepareLink(Self, User);
    end;
end;

procedure TUser.ExecGames(S: String);
const Command='GAMES';
var
  I: Integer;
  OpenType: String;
begin                                 
  SendLn(':SERVER'#160'GAMES!root@'+ServerHost+' PRIVMSG '+Nickname+' :--- Channel | Passworded | Name | Hoster | URL ---');
  for I:=0 to Length(Games)-1 do
  with Games[I] do
  begin
    if PassNeeded='0' then OpenType:='[OPEN]'
    else OpenType:='[PASS]';
    SendLn(':SERVER'#160'GAMES!root@'+ServerHost+' PRIVMSG '+Nickname+' :#'+Chan+' | '+OpenType+' | '+Name+' | '+HosterNickname+' | wa://'+HosterAddress+'?gameid='+IntToStr(GameID)+'&Scheme='+Scheme);
  end;
  SendLn(':SERVER'#160'GAMES!root@'+ServerHost+' PRIVMSG '+Nickname+' :--- '+IntToStr(Length(Games))+' games total ---');
end;

procedure TUser.SendLn(S: string);
var TStr: String;
begin
  if Socket=0 then Exit;
  TStr:='[IRC] > '+S;
  if TStr <> LastStr then Log(TStr);
  LastStr:=TStr;
  S:=S+#13#10;
  if send(Socket, S[1], Length(S), 0)<>Length(S) then
    begin
    Socket:=0;  // avoid infinite recursion
    Log('[IRC] > Failed ('+WinSockErrorCodeStr(WSAGetLastError)+')');
    end;
end;

function TUser.Registered: Boolean;
begin
  if Nickname='' then
    Result:=false
  else
    Result:=true;
end;

function TUser.ChangeMode(Side, Mode: Char; Master: String): Boolean;
var
  I, J: Integer;
  Pre: String;
begin
  if ((Side = '+') and not (Modes[Mode])) or ((Side = '-') and (Modes[Mode])) then
  begin

    if Side='+' then Modes[Mode] := true
    else Modes[Mode] := false;

    if Mode='b' then
    begin
      LastSenior:=Master;
      if Side = '+' then
      begin
        Pre:='';
        LastBanTime:=IRCDateTimeNow;
      end
      else
        Pre:='un';
      SendLn(':SERVER'#160'MESSAGE!root@'+ServerHost+' PRIVMSG '+Nickname+' :You have been '+Pre+'muted by '+Master+'.');
      EventLog(Nickname+' has been '+Pre+'muted by '+Master+'.');
    end

    else
      if Mode='i' then
      begin
        if Side = '+' then Pre:='in'
        else Pre:='';
        SendLn(':SERVER'#160'MESSAGE!root@'+ServerHost+' PRIVMSG '+Nickname+' :You have been made '+Pre+'visible by '+Master+'.');
        EventLog(Nickname+' has been made '+Pre+'visible by '+Master+'.');
      end;

    for I:=0 to Length(Channels)-1 do
      if InChannel[Channels[I].Number] then
        for J:=0 to Length(Users)-1 do
          if Users[J].InChannel[Channels[I].Number] then
            if Mode='i' then
            begin
              if Nickname <> Users[J].Nickname then
                if Side = '+' then
                  Users[J].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+Channels[I].Name)
                else
                  Users[J].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN '+Channels[I].Name);
            end
            else
            begin
              Users[J].SendLn(':'+Master+' MODE '+Channels[I].Name+' '+Side+Mode+' '+Nickname);
            end;

    if (Mode<>'b')and(Mode<>'i') then
      EventLog(Master+' has set mode '+Side+Mode+' to '+Nickname+'.');

    Result:=true;
  end
  else
    Result:=false;
end;

constructor TChannel.Create(Name, Scheme, Topic: String);
begin
    Name:=Name;
    Scheme:=Scheme;
    Topic:=Topic;
    Number:=Length(Channels)-1;
end;

destructor TChannel.Destroy;
var I: Integer;
begin
  for I:=Number to Length(Channels)-2 do
    Channels[I]:=Channels[I+1];
  SetLength(Channels, Length(Channels)-1);
end;

// ***************************************************************

procedure GetChannels;
var
  Chanfile: TMemIniFile;
  Name, Scheme, Topic: String;
  FilePath: String;
  N: Integer;
begin
  FilePath := ExtractFilePath(ParamStr(0))+'Channels.ini';
  ChanFile := TMemIniFile.Create(FilePath);
  if not (FileExists(FilePath)) then
  begin
    EventLog('[IRC] Could not find the Channels.ini file: default channel will be created.');
    AddChannel('#AnythingGoes','Pf,Be','00 Open games with ''rope knocking'' allowed & blood fx');
  end
  else
  begin
    N:=1;
    while ChanFile.ReadString(IntToStr(N), 'Name', 'ENDOFLIST') <> 'ENDOFLIST' do
    begin
      Name   :=ChanFile.ReadString (IntToStr(N),'Name',   '');
      Scheme :=ChanFile.ReadString (IntToStr(N),'Scheme', '');
      Topic  :=ChanFile.ReadString (IntToStr(N),'Topic',  '');

      Name   := Copy(Name,1,Pos(' ',Name+' ')-1);
      Scheme := Copy(Scheme,1,Pos(' ',Scheme+' ')-1);
      if Name[1] <> '#' then Name:='#'+Name;
      
      if not AddChannel(Name,Scheme,Topic) then
        EventLog('[IRC] Channel '+Name+' has already been created: ignored.')
      else             
        EventLog('[IRC] Channel '+Name+' has been added.');
      Inc(N);
    end;
    if N=1 then
    begin 
      EventLog('[IRC] No channels found in the Channels.ini file: default channel will be created.');
      AddChannel('#AnythingGoes','Pf,Be','00 Open games with ''rope knocking'' allowed & blood fx');
    end;
  end;
end;

function AddChannel(Name, Scheme, Topic: String): Boolean;
var
  I: Integer;
  B: Boolean;
  Channel: TChannel;
begin
  B:=true;
  for I:=0 to Length(Channels)-1 do
    if Channels[I].Name = Name then
    begin
      B:=false;
      Break;
    end;
  if (B) then
  begin
    SetLength(Channels, Length(Channels)+1);
    Channel:=TChannel.Create(Name, Scheme, Topic);
    Channels[Length(Channels)-1]:=Channel;
    Channels[Length(Channels)-1].Name:=Name;
    Channels[Length(Channels)-1].Scheme:=Scheme;
    Channels[Length(Channels)-1].Topic:=Topic;
    Channels[Length(Channels)-1].Number:=Length(Channels)-1;
  end;
  Result:=B;
end;

function ChannelByName(Name: String): TChannel;
var
  I: Integer;
begin
  Result := nil;
//  EventLog('[IRC] Verifying channel '+Name+'...');
  for I:=0 to Length(Channels)-1 do
    if Name <> '' then
      if UpperCase(Channels[I].Name) = UpperCase(Name) then
      begin
        Result := Channels[I];
        Break;
      end;
end;

function NickInUse(Nick: String): Boolean;
var I: Integer;
begin
  Result := False;
  for I:=0 to Length(Users)-1 do
    if UpperCase(Users[I].Nickname)=UpperCase(Nick) then
      Result := True;
end;

function MainProc(Nothing: Pointer): Integer; stdcall;
var
  m_socket, AcceptSocket: TSocket;
  service, incoming: TSockAddrIn;
  T: Integer;
  User: TUser;
begin
  Result:=0;
  m_socket := socket( AF_INET, SOCK_STREAM, IPPROTO_TCP );

  service.sin_family := AF_INET;
  service.sin_addr.s_addr := inet_addr( '0.0.0.0' );
  service.sin_port := htons( IRCPort );

  if bind(m_socket, service, sizeof(service))=SOCKET_ERROR then
    begin
    EventLog('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    EventLog('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  EventLog('[IRC] Listening on port '+IntToStr(IRCPort)+'.');

  GetChannels;

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if AcceptSocket<>INVALID_SOCKET then
      begin
      T:=SizeOf(incoming);
      Log('[IRC] Connection established from '+inet_ntoa(incoming.sin_addr));

      User:=TUser.Create(True);
      User.Socket:=AcceptSocket;
      User.ConnectingFrom:=inet_ntoa(incoming.sin_addr);
//      User.Modes['s']:=True;
      SetLength(Users, Length(Users)+1);
      Users[Length(Users)-1]:=User;
      User.Resume;
      end
    else
      Sleep(5);
  until False;
end;

procedure LogToOper(S: string);
var
  I: Integer;
begin
  for I:=0 to Length(Users)-1 do
    with Users[I] do
    if (Modes['L']) and ((Modes['o']) or (Modes['a']) or (Modes['q'])) then
      SendLn(':'+ServerHost+' NOTICE '+Nickname+' :'+S);
end;

var
  ThreadID: Cardinal = 0;

procedure StartIRCServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

end.
