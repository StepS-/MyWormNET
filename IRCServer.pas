unit IRCServer;

{$I cDefines.inc}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface
uses
{$IFDEF OS_MSWIN)}
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
    SignonTime, LastBanTime: Int64;
    Socket: TSocket;
    InChannel: array of Boolean;
    Modes: array[char] of Boolean;

    procedure Execute; override;
    procedure SendLn(S: string);
    procedure LogIn(S: String);

    procedure ExecPing(S: String);
    procedure ExecNick(S: String);
    procedure ExecUser(S: String);
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
    procedure ExecWhois(S: String);
    procedure ExecMotd(S: String);
    procedure ExecBan(Command, S: String);
    procedure ExecOper(Command, S: String);
    procedure ExecMessage(Command, S: String);
    procedure ExecMute(Mute: Boolean; S: String);

    procedure Die(Action, Reason, Master: String);
    function Registered: Boolean;
    function ChangeMode(Side, Mode: Char; Master: String): Boolean;
     
    function SendEvent(EventNo: Integer; S: String): String;
    function SendError(ErrNo: Integer; S: String): String;
    function ServerMessage(S: String; MessageType: ShortInt=0): String;
    end;

  TChannel=class (TObject)
    Name, Scheme, Topic: String;
    Number: Integer;
    CreationTime: Int64;

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
function UserByName(Name: String): TUser;
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
  BufferA, SA: ansistring;
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
    BufferA:='';
    Buffer:='';
    PingTimer:=0;
    Password:='';
    repeat
      repeat
        BufferA:='';
        SA:='';
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

        SetLength(SA, Bytes);
        R:=recv(Socket, SA[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
          begin
          Log('[IRC] '+ConnectingFrom+' Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create('Connection error ('+WinSockErrorCodeStr(WSAGetLastError)+')');
          end;
        SetLength(SA, R);
        BufferA := BufferA + SA;
        Buffer := String(BufferA);
        S := String(SA);
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

          if Command='CAP' then
        else
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
            ExecPing(S)
        else
          if Command='PONG' then
        else
          if Registered then
            if Command='LIST' then
              ExecList(S)
          else
            if Command='WHO' then
              ExecWho(S)
          else
            if Command='ISON' then
              ExecIson(S)
          else
            if Command='JOIN' then
              ExecJoin(S)
          else
            if Command='PART' then
              ExecPart(S)
          else
            if Command='QUIT' then
              ExecQuit(S)
          else
            if Command='MODE' then
              ExecMode(S)
          else
            if Command='GAMES' then
              ExecGames(S)
          else
            if Command='NAMES' then
              ExecNames(S)
          else
            if Command='MOTD' then
              ExecMotd(S)
          else
            if Command='EXPECT' then
              ExecExpect(S)
          else
            if Command='ANNOUNCE' then
              ExecAnnounce(S)
          else
            if Command='PRANK' then
              ExecPrank(S)
          else
            if Command='SENDRAW' then
              ExecSendraw(S)
          else
            if Command='KICKALL' then
              ExecKickall(S)
          else
            if (Command='WHOIS')or(Command='IPLOOKUP') then
              ExecWhois(S)
          else
            if (Command='PRIVMSG') or (Command='NOTICE') then
              ExecMessage(Command,S)
          else
            if (Command='KICK') or (Command='KILL') then
              ExecKill(S)
          else
            if (Command='PERMABAN') or (Command='REMOVEBAN') then
              ExecBan(Command,S)
          else
            if (Command='OPER') or (Command='TAKEOWN') then
              ExecOper(Command,S)
          else
            if Command='MUTE' then
              ExecMute(true,S)
          else
            if Command='UNMUTE' then
              ExecMute(false,S)
          else
            if Command='TIME' then
              SendEvent(391, ServerHost+' :'+TextDateTimeNow)
          else
            if Command='AWAY' then
          else
            SendError(421,Command)
        else
          if (Command<>'') then
            SendError(451,Command);
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

  if Registered then
    EventLog(Nickname+' ('+ConnectingFrom+') has disconnected.')
  else
    EventLog('<Unknown> ('+ConnectingFrom+') has connected and then disconnected.');

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
begin
  if not BannedNick(Nickname) then
  begin
    EventLog(Nickname+' ('+ConnectingFrom+') has logged in.');
    SendEvent(001, ':Welcome, '+Nickname+'!');
    SendEvent(011, ':This is a custom WormNet IRC server emulator,');
    SendEvent(011, ':supporting custom set of IRC features.');
    SendEvent(011, ':The server software was written by ');
    SendEvent(011, ':The_CyberShadow <thecybershadow@gmail.com>');
    SendEvent(011, ':and extended by StepS <github.com/StepS->');
    if WormNATPort>0 then
      SendEvent(011, ':[WormNATRouteOn:'+IntToStr(WormNATPort)+'] This server supports built-in WormNAT routing.');
    SendEvent(002, ':Your host is '+ServerHost+', running MyWormNET version '+APPVERSION);
    SendEvent(003, ':This server was created '+StartupTime);
    SendEvent(004, ServerHost+' '+APPVERSION+' biL'+IRCPrefModes+' tnb');
    SendEvent(005, 'WALLCHOPS PREFIX=('+IRCPrefModes+')'+IRCPrefixes+' STATUSMSG='+IRCPrefixes+' CHANTYPES=# NICKLEN=15 NETWORK='+NetworkName+' CHANMODES=b,nt MODES=2 :are supported by this server');
    N:=0;
    M:=0;
    for I:=0 to Length(Users)-1 do
      begin
      if (Users[I].Modes['q'])or(Users[I].Modes['a'])or(Users[I].Modes['o']) then
        Inc(N);
      if Users[I].Modes['i'] then
        Inc(M);
      end;
    SendEvent(251, ':There are '+IntToStr(Length(Users)-M)+' users and '+IntToStr(M)+' invisible on this server.');
    SendEvent(252, IntToStr(N)+' :IRC Operators online');
    SendEvent(254, Nickname+' '+IntToStr(Length(Channels))+' :channels on this server');
    ExecMotd(S);
  end
  else
  begin
    SendLn('ERROR :You are banned.');
    EventLog(Nickname+' ('+ConnectingFrom+') has been halted due to his nick being banned.');
    closesocket(Socket);
    Socket:=0
  end;
end;

function TUser.SendEvent(EventNo: Integer; S: String): String;
var EventCode: String;
begin
  EventCode:=IntToStr(EventNo);
  while Length(EventCode) < 3 do
    EventCode:='0'+EventCode;
  Result:=':'+ServerHost+' '+EventCode+' '+Nickname+' '+S;
  SendLn(Result);
end;

function TUser.SendError(ErrNo: Integer; S: String): String;
var StrOut: String;
begin
  case ErrNo of
  400:
    StrOut:='Nick change isn''t supported';
  401:
    StrOut:='No such nick/channel';
  403:
    StrOut:='No such channel';
  404:
    StrOut:='Cannot send to channel';
  412:
    StrOut:='No text to send';
  421:
    StrOut:='Unknown command';
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

  Result:=':'+ServerHost+' '+IntToStr(ErrNo)+' '+Nickname+' '+S+' :'+StrOut;
  SendLn(Result);
end;

function TUser.ServerMessage(S: String; MessageType: ShortInt=0): String;
var
  Sender: String;
begin
  case MessageType of
  0:
    Sender:='SERVER'#160'MESSAGE';
  else
    Sender:='SERVER'#160'GAMES';
  end;
    
  Result:=':'+Sender+'!root@'+ServerHost+' PRIVMSG '+Nickname+' :'+S;
  SendLn(Result);
end;

{procedure TUser.ExecIpLookup(S: String);
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
        ServerMessage('IP of user '+S+' is: '+Users[I].ConnectingFrom+'.');
        Break
      end
      else if I=Length(Users)-1 then
        SendError(401,S);
    end
    else
      SendError(481,Command);
  end
  else
    SendError(461,Command);
end;}

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
              SendEvent(401, S+' :This user has '+Pre+' been muted');
              Break
            end;

            Break;
          end
          else
          begin
            SendError(484,S);
            Break
          end;
        end
        else if I=Length(Users)-1 then
          SendError(401,S);
    end
    else
      SendError(481,Command);
  end
  else
    SendError(461,Command);
end;

procedure TUser.ExecKill(S: String);
const Command='KILL';
var
  I: Integer;
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
        if Reason[1] = ':' then Delete(Reason, 1, 1);
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
                Users[I].Die('killed',Reason,Nickname);
                Break
              end
            else
              begin
                SendError(484,Target);
                Break
              end;
          end
        else if I=Length(Users)-1 then
          SendError(401,Target);
     end
    else
      begin
      SendError(481,Command);
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
    SendError(461,Command);
end;

procedure TUser.ExecWhois(S: String);
const Command='WHOIS';
var
  I: Integer;
  UserChannels, UserPrefixes: String;
  User: TUser;
begin
  S:=Copy(S,1,Pos(' ',S+' ')-1);
  UserChannels:='';
  UserPrefixes:='';
  User:=UserByName(S);
  if User <> nil then
  begin
    for I:=1 to Length(IRCPrefModes) do
      if User.Modes[IRCPrefModes[I]] then
        UserPrefixes:=UserPrefixes+IRCPrefixes[I];

    for I:=0 to Length(Channels)-1 do
      if User.InChannel[I] then
        UserChannels:=UserChannels+UserPrefixes+Channels[I].Name+' ';

    SendEvent(311, User.Nickname+' '+User.Username+' '+StealthIP+' * :'+User.Realname);
//  SendEvent(307, User.Nickname+' :is a registered nick');
    if UserChannels<>'' then
      SendEvent(319, User.Nickname+' :'+UserChannels);
    SendEvent(312, User.Nickname+' '+ServerHost+' :'+NetworkName+' (MyWormNET '+APPVERSION+')');
    if (User = Self) or (Modes['o']) or (Modes['a']) or (Modes['q']) then
      SendEvent(338, User.Nickname+' '+User.ConnectingFrom+' :Actual IP');
    SendEvent(317, User.Nickname+' 0 '+IntToStr(User.SignonTime)+' :seconds idle, signon time');

  end;
  SendEvent(318, S+' :End of /WHOIS list.');
end;

procedure TUser.ExecMotd(S: String);
const Command='MOTD';
var
  Buf: String;
begin
  SendEvent(375, ':- '+NetworkName+' Message of the Day - ');
  S:=GetFile('motd.txt')+#13#10;
  while GetLine(S, Buf) do
    if(S<>'')or(Buf<>'') then
      SendEvent(372, ':- '+Buf);
  SendEvent(376, ':End of /MOTD command.');
end;

procedure TUser.ExecBan(Command, S: String);
var
  I, J: Integer;
  B: Boolean;
  F: text;
  FilePath, BanFile: String;
  Target, Reason, BType, Result: String;
begin
  if (Modes['o']) or (Modes['a']) or (Modes['q']) then
  begin
    if S<>'' then
    begin
      B:=true;
      if Pos(' ', S) <> 0 then
      begin
        Target:=Copy(S, 1, Pos(' ', S)-1);
        Reason:=Copy(S, Pos(' ', S)+1, Length(S));
        if Reason[1] = ':' then Delete(Reason, 1, 1);
      end
      else
      begin
        Target:=Copy(S, 1, Length(S));
        Reason:='No reason specified';
      end;

      if Command='PERMABAN' then
      begin
        if (Pos('.',Target) = 0) and (Pos(':',Target) = 0) then
        begin
          BanFile:='banlist_nicks.txt';
          BType:='Nickname';
          Result:='banned';
          for I:=0 to Length(NickBans)-1 do
            if UpperCase(NickBans[I]) = UpperCase(Target) then
            begin
              B:=false;
              Break;
            end;
          if (B) then
          begin
            SetLength(NickBans,Length(NickBans)+1);
            NickBans[Length(NickBans)-1]:=Target;
          end;
        end
        else
        begin
          BanFile:='banlist_ips.txt';
          BType:='IP';
          Result:='permabanned';
          for I:=0 to Length(IPBans)-1 do
            if UpperCase(IPBans[I]) = UpperCase(Target) then
            begin
              B:=false;
              Break;
            end;
          if (B) then
          begin
            SetLength(IPBans,Length(IPBans)+1);
            IPBans[Length(IPBans)-1]:=Target;
          end;
        end;

        if B then
        begin
          FilePath := ExtractFilePath(ParamStr(0))+BanFile;
          Assign(F,FilePath);
          if not FileExists(FilePath) then
            Rewrite(F);
          Append(F);
          WriteLn(F, Target);
          Close(F);

          for I:=0 to Length(Users)-1 do
            if BType='Nickname' then
            begin
              if UpperCase(Users[I].Nickname) = UpperCase(Target) then
              begin
                Users[I].Die(Result,Reason,Nickname);
                Break;
              end;
            end
            else
              if UpperCase(Users[I].ConnectingFrom) = UpperCase(Target) then
              begin
                Users[I].Die(Result,Reason,Nickname);
                Break;
              end;
        end;
      end
      else
      begin
        Result:='unbanned';
        if (Pos('.',Target) = 0) and (Pos(':',Target) = 0) then
        begin
          BanFile:='banlist_nicks.txt';
          BType:='Nickname';
          for I:=Length(NickBans)-1 downto 0 do
            if UpperCase(NickBans[I]) = UpperCase(Target) then
            begin
              for J:=I to Length(NickBans)-2 do
                NickBans[J]:=NickBans[J+1];
              Break;
            end
            else if I=0 then B:=false;
          if B then
            SetLength(NickBans,Length(NickBans)-1);
        end
        else
        begin
          BanFile:='banlist_ips.txt';
          BType:='IP';
          for I:=Length(IPBans)-1 downto 0 do
            if UpperCase(IPBans[I]) = UpperCase(Target) then
            begin
              for J:=I to Length(IPBans)-2 do
                IPBans[J]:=IPBans[J+1];
              Break;
            end
            else if I=0 then B:=false;
          if B then
            SetLength(IPBans,Length(IPBans)-1);
        end;

        if B then
        begin
          FilePath := ExtractFilePath(ParamStr(0))+BanFile;
          Assign(F,FilePath);
          Rewrite(F);
          if BType='Nickname' then
            for I:=0 to Length(NickBans)-1 do
              WriteLn(F,NickBans[I])
          else
            for I:=0 to Length(IPBans)-1 do
              WriteLn(F,IPBans[I]);
          Close(F);
        end;
      end;

      if B then
      begin
        EventLog(Nickname+' has '+Result+' '+LowerFCStr(BType)+' "'+Target+'".');
        ServerMessage(BType+' "'+Target+'" has been '+Result+'.')
      end
      else ServerMessage(BType+' '+Target+' has already been '+Result+'.')
    end
    else
      SendError(461,Command);
  end
  else
    SendError(481,Command);
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
          SendError(484,Target);
          Break
        end;
      end
  else if I=Length(Users)-1 then
    SendError(401,Target);
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
    SendError(481,Command);
end;

procedure TUser.ExecSendraw(S: String);
const Command='SENDRAW';
var I: Integer;
begin
  if (Modes['q']) then
  begin
    if (S='') then
      SendError(461,Command)
    else
      for I:=0 to Length(Users)-1 do
        Users[I].SendLn(S);
  end
  else
    SendError(481,Command);
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
            Users[J].SendLn(':SERVER'#160'ANNOUNCEMENT!root@'+ServerHost+' NOTICE '+Channels[I].Name+' :'+S);
      EventLog(Nickname+' has made a global announcement: "'+S+'".');
    end
    else
      SendError(412,Command);
  end
  else
    SendError(481,Command);
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
      SendEvent(303, ':'+IsonBuff)
    else
    begin
      Target:=S;
      for I:=0 to Length(Users)-1 do
        if Target=Users[I].Nickname then
          IsonBuff:=IsonBuff+Target;
      SendEvent(303, ':'+IsonBuff);
    end;
  end;
end;

procedure TUser.ExecPing(S: String);
const Command='PING';
begin
  if S<>'' then
    SendLn(':'+ServerHost+' PONG '+ServerHost+' :'+S)
  else
    SendLn('PONG :'+ServerHost);
end;

procedure TUser.ExecNick(S: String);
const Command='NICK';
var I: Integer;
begin
  if Nickname<>'' then
    SendError(400,S)
  else
  begin
    for I:=Length(S) downto 1 do
      if Pos(S[I], ValidNickChars)=0 then
        Delete(S, I, 1);
        
    if Length(S) > 15 then
    repeat
      Delete(S, Length(S), 1);
    until Length(S) = 15;

    if S='' then
      SendError(432,Command)
    else
    begin
      if NickInUse(S) then
      SendError(433,S)
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

      if Username='' then
        Username:='Username'; //Prevent the Username from being blank (i.e. Wheat Snooper)
      if Nickname<>'' then
        LogIn(S);
    end
    else
      SendError(462,Command);
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
    StrOut:='= '+S+' :';
    for I:=0 to Length(Users)-1 do
      if (Users[I].InChannel[N]) and not (Users[I].Modes['i']) then
      begin
        for K:=1 to Length(IRCPrefModes) do
          if Users[I].Modes[IRCPrefModes[K]] then
            StrOut:=StrOut+IRCPrefixes[K];
        StrOut:=StrOut+Users[I].Nickname+' ';
      end;
    SendEvent(353, StrOut);
  end;
  SendEvent(366, S+' :End of /NAMES list.');
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
      SendEvent(332, Channel.Name+' :'+Channel.Topic);
      SendEvent(333, Channel.Name+' '+ServerHost+' '+IntToStr(Channel.CreationTime));
      ExecNames(CurChan);
    end
    else
      SendError(403,CurChan);
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
                              SendError(502,Command)
                            else
                            begin
                              SendError(482,Command);
                              Break;
                            end;
                        end;
                      end;
                    Break
                    end
                    else if I = Length(Users)-1 then
                      SendError(401,Target);
                  end;
                end
                else
                  SendError(481,Command);
              end
              else
                if Pos('+b',S) <> 0 then
                begin
                  for I:=0 to Length(Users)-1 do
                    if Users[I].Modes['b'] then
                      SendEvent(367, Channel.Name+' '+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' '+Users[I].LastSenior+' '+IntToStr(Users[I].LastBanTime));
                SendEvent(368, Channel.Name+' :End of Channel Ban List');
                end;
            end
            else
            begin
              SendEvent(324, Channel.Name+' +tn');
              SendEvent(329, Channel.Name+' '+IntToStr(Channel.CreationTime));
            end;
          end
          else
          begin
            SendEvent(324, Channel.Name+' +tn');
            SendEvent(329, Channel.Name+' '+IntToStr(Channel.CreationTime));
          end;
        end
        else
          SendError(403,Target);
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
    ServerMessage('Sorry, but you are muted by '+LastSenior+' and thus cannot talk.')
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
        SendError(401,Target)
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
    SendEvent(381,':You are now an IRC '+Description);
  end
  else
    SendError(464,Command);
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
            SendEvent(352, ChanStr+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Target+' H'+Pref+' :0 '+Users[I].Realname);
      end
      else
        SendEvent(352, ChanStr+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Target+' H'+Pref+' :0 '+Realname);
      if (S<>'') and (S=Users[I].Nickname) then Break;
    end
    else if Users[I].InChannel[Channel.Number] then
      begin
      if (Pos(' o ',S+' ') <> 0) and not ((Users[I].Modes['h']) or (Users[I].Modes['o']) or (Users[I].Modes['a']) or (Users[I].Modes['q']))
        then continue
      else
      if not (Users[I].Modes['i']) then
        if Users[I].Nickname <> Nickname then
          SendEvent(352, Channel.Name+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Users[I].Nickname+' H'+Pref+' :0 '+Users[I].Realname)
        else
          SendEvent(352, Channel.Name+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Nickname+' H'+Pref+' :0 '+Realname);
      end;
  end;
  if S='' then
    SendEvent(315, '* :End of /WHO list.')
  else
    SendEvent(315, S+' :End of /WHO list.');
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
    SendEvent(322, Channels[I].Name+' '+IntToStr(N)+' :'+Channels[I].Topic);
  end;
  SendEvent(323, ':End of /LIST');
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
      SendError(401,S)
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
  ServerMessage('--- Channel Passworded Name Hoster URL ---', 1);
  for I:=0 to Length(Games)-1 do
  with Games[I] do
  begin
    if PassNeeded='0' then OpenType:='[OPEN]'
    else OpenType:='[PASS]';
    ServerMessage('#'+Chan+' '+OpenType+' '+Name+' '+HosterNickname+' wa://'+HosterAddress+'?gameid='+IntToStr(GameID)+'&Scheme='+Scheme, 1);
  end;
  ServerMessage('--- '+IntToStr(Length(Games))+' games total ---', 1);
end;

procedure TUser.SendLn(S: string);
var
  TStr: String;
  AStr: AnsiString;
begin
  if Socket=0 then Exit;
  TStr:='[IRC] > '+S;
  if TStr <> LastStr then Log(TStr);
  LastStr:=TStr;
  AStr:=AnsiString(S);
  AStr:=AStr+#13#10;
  if send(Socket, AStr[1], Length(AStr), 0)<>Length(AStr) then
  begin
    Socket:=0;  // avoid infinite recursion
    Log('[IRC] > Failed ('+WinSockErrorCodeStr(WSAGetLastError)+')');
  end;
end;

procedure TUser.Die(Action, Reason, Master: String);
var I: Integer;
begin
  LastSenior:=Master;
  for I:=0 to Length(Channels)-1 do
    if InChannel[Channels[I].Number] then
      InChannel[Channels[I].Number] := False;
  if not Modes['i'] then
    for I:=0 to Length(Users)-1 do
      Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+UpperFCStr(Action)+' by '+Master+': '+Reason);
  SendLn('ERROR :You have been '+LowerFCStr(Action)+' by '+Master+': '+Reason);
  EventLog(Nickname+' has been '+LowerFCStr(Action)+' by '+Master+': '+Reason);
  if (Socket <> 0) then closesocket(Socket); Socket:=0;
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
      ServerMessage('You have been '+Pre+'muted by '+Master+'.');
      EventLog(Nickname+' has been '+Pre+'muted by '+Master+'.');
    end

    else
      if Mode='i' then
      begin
        if Side = '+' then Pre:='in'
        else Pre:='';
        ServerMessage('You have been made '+Pre+'visible by '+Master+'.');
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
    CreationTime:=IRCDateTimeNow;
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
    EventLog('[IRC] Channel #AnythingGoes has been added.');
  end
  else
  begin
    N:=1;
    while ChanFile.ReadString(IntToStr(N), 'Name', 'ENDOFLIST') <> 'ENDOFLIST' do
    begin
      Name   :=ChanFile.ReadString (IntToStr(N),'Name',   '');
      Scheme :=ChanFile.ReadString (IntToStr(N),'Scheme', 'Pf,Be');
      Topic  :=ChanFile.ReadString (IntToStr(N),'Topic',  '00');

      while Pos(' ',Name) <> 0 do
        Delete(Name,Pos(' ',Name),1);
      while Pos(' ',Scheme) <> 0 do
        Delete(Scheme,Pos(' ',Scheme),1);

      if (Name='') or (Name='#') then Name:='#AnythingGoes';
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
      EventLog('[IRC] Channel #AnythingGoes has been added.');
    end;
  end;
end;

function AddChannel(Name, Scheme, Topic: String): Boolean;
var
  I, L: Integer;
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
    L:=Length(Channels)-1;
    Channels[L]:=Channel;
    Channels[L].Name:=Name;
    Channels[L].Scheme:=Scheme;
    Channels[L].Topic:=Topic;
    Channels[L].CreationTime:=Channel.CreationTime;
    Channels[L].Number:=L;
  end;
  Result:=B;
end;

function UserByName(Name: String): TUser;
var
  I: Integer;
begin
  Result := nil;
  for I:=0 to Length(Users)-1 do
    if Name <> '' then
      if UpperCase(Users[I].Nickname) = UpperCase(Name) then
      begin
        Result := Users[I];
        Break;
      end;
end;

function ChannelByName(Name: String): TChannel;
var
  I: Integer;
begin
  Result := nil;
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
    if (AcceptSocket<>INVALID_SOCKET) then
    begin
      if not BannedIP(String(inet_ntoa(incoming.sin_addr))) then
      begin
        T:=SizeOf(incoming);
        Log('[IRC] Connection established from '+inet_ntoa(incoming.sin_addr));

        User:=TUser.Create(true);
        SetLength(User.InChannel,Length(Channels));
        User.SignonTime:=IRCDateTimeNow;
        User.Socket:=AcceptSocket;
        User.ConnectingFrom:=String(inet_ntoa(incoming.sin_addr));
        User.LastSenior:='SERVER';
    //  User.Modes['s']:=True;
        SetLength(Users, Length(Users)+1);
        Users[Length(Users)-1]:=User;
        {$IFDEF DELPHI2010_UP}
        User.Start;
        {$ELSE}
        User.Resume;
        {$ENDIF}
      end
      else
      begin
        EventLog('Rejected request from banned IP '+inet_ntoa(incoming.sin_addr)+' to port '+IntToStr(IRCPort)+'.');
        closesocket(AcceptSocket);
        Sleep(5);
      end;
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
