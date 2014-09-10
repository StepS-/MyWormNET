unit IRCServer;


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
  Classes;

type
  TTopic=record
      Text, SetBy: String;
      TimeSet: Int64;
    end;

  TUserCount=record
      Operators, Invisible, Unknown,
      Registered, All: Integer;
    end;

  TSeen=class (TObject)
    public
      Nick, QuitMsg: String;
      SilentQuit: Boolean;
      LastSeen: TDateTime;

      constructor Create(SeenNick, SeenQuitMsg: String; SeenSilentQuit: Boolean; SeenLastSeen: TDateTime);
      destructor Destroy; override;
    end;

  TChannel=class (TObject)
    private
      function GetUserCount: u_int;

    public
      Name, Scheme: String;
      Topic: TTopic;
      property UserCount: u_int read GetUserCount;
      constructor Create(ChanName, ChanScheme, ChanTopic, ChanCreatedBy: String);
      destructor Destroy; override;
    end;

  TSuper=class (TObject)
    public
      Name, IP: String;
      constructor Create(SuperName, SuperIP: String);
      destructor Destroy; override;
    end;

  TUser=class (TThread)
    private
      Socket: TSocket;
      Away, White, Quit, SilentQuit: Boolean;
      UserPass: string;
      LastSenior: string;
      AwayMsg, QuitMsg: string;
      TempNick, TempUsername: string;
      FloodPoints, DataStats: Integer;
      SignonTime, LastBanTime: Int64;
      LastMessageTime, LastDataTime: TDateTime;
      function IsAuthorized: Boolean;
      function IsRegistered: Boolean;

      procedure LogIn(S: String);

      procedure ExecPing(S: String);
      procedure ExecNick(S: String);
      procedure ExecUser(S: String);
      procedure ExecPass(S: String);
      procedure ExecPrank(S: String);
      procedure ExecKickall(S: String);
      procedure ExecSendfrom(S: String);
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
      procedure ExecKick(S: String);
      procedure ExecKill(S: String);
      procedure ExecWhois(S: String);
      procedure ExecInfo(S: String);
      procedure ExecMotd(S: String);
      procedure ExecLusers(S: String);
      procedure ExecUsers(S: String);
      procedure ExecSeen(S: String);
      procedure ExecAway(S: String);
      procedure ExecTopic(S: String);
      procedure ExecCalc(S: String);
      procedure ExecAuthpong(S: String);
      procedure ExecForcegameid(S: String);
      procedure ExecAddchannel(S: String);
      procedure ExecRemovechannel(S: String);
      procedure ExecReload(S: String);
      procedure ExecShutdown(S: String);
      procedure ExecBan(Command, S: String);
      procedure ExecOper(Command, S: String);
      procedure ExecMessage(Command, S: String);
      procedure ExecMute(Mute: Boolean; S: String);

      procedure GetGameVersion;
      procedure GetSafeRealname;
      procedure AddSeen;
      function MessageFloodCheck(MsgLen: Integer): Boolean;
      function DataFloodCheck(DataLen: Integer): Boolean;
      function ContextCommand(S: String): Boolean;

    public
      Nickname, Username, Hostname, Servername, Realname, SafeRealname: string;
      GameVersion: TVersion;
      ConnectingFrom: string;
      ChannelsJoined: TThreadList;
      Modes: array[char] of Boolean;

      property Authorized: Boolean read IsAuthorized;
      property Registered: Boolean read IsRegistered;

      procedure Execute; override;
      procedure ResumeThread;

      procedure SendLn(Source: TUser; Msg: string; Logging: Boolean=true); overload;
      procedure SendLn(S: string; Logging: Boolean=true); overload;
      procedure Broadcast(Msg: string; Channel: TChannel; FullString: Boolean = false; ExceptSelf: Boolean=false; Logging: Boolean = true); overload;
      procedure Broadcast(Msg: string; FullString: Boolean = false; OnlyChannels: Boolean = True; ExceptSelf: Boolean=false; Logging: Boolean = True); overload;

      procedure ExecuteCommand(S: String);
      procedure Kill(Killer: TUser; Reason: String);
      procedure ServerKill(Reason: String);
      procedure CloseConnection;

      function InChannel(Channel: TChannel): Boolean;

      function ChangeMode(Side, Mode: Char; Master: TUser): Boolean;

      function SendEvent(EventNo: Integer; S: String; Logging: Boolean=true): String;
      function SendError(ErrNo: Integer; S: String; Logging: Boolean=true): String;
      function RuptureError(Comment: String): String;
      function ServerMessage(S: String; MessageType: ShortInt=0): String;
    end;

const
  IRCPrefModes='qaohv';
  IRCPrefixes='~&@%+';
  IRCPassword='ELSILRACLIHP ';
  IRCPassword2='ELSILRACLIHP';
  ValidNickChars='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`-';

var
  UserThreadList: TThreadList;
  ChannelThreadList: TThreadList;
  SeenThreadList: TThreadList;
  SuperThreadList: TThreadList;

procedure StartIRCServer;
procedure GetChannels;
procedure LogToOper(S: string);
procedure AddSeen(Nick, QuitMsg: String);
function AddChannel(Name, Scheme, Topic: String): Boolean;
function GetFormattedUserCount: TUserCount;
function GetRegisteredUserCount: u_int;
function GetUserCount: u_int;
function GetChannelCount: u_int;
function LockUserByIP(IP: String): TUser;
function LockUserByName(Name: String): TUser;
function LockChannelByName(Name: String): TChannel;
function LockSuperByIP(IP: String): TSuper;
function LockSuperByName(Name: String): TSuper;
function SuperNameExists(Name: String): Boolean;
function SuperIPExists(IP: String): Boolean;
function NickInUse(Nick: string): Boolean;
function ChannelExists(Name: string): Boolean;
function ForbiddenNick(Nick: string): Boolean;

implementation
uses
  Base, Data, DateUtils, SysUtils, IniFiles, HTTPServer, WormNATServer;

procedure TUser.Execute;
var
  BufferA, SA: ansistring;
  Buffer, S: string;
  R, Bytes, I, J, N: Integer;
  PingTimer: Integer;
  RegTimer: Integer;
  ReadSet, ErrorSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;

begin
  try
    Buffer:='';
    BufferA:='';
    PingTimer:=0;
    RegTimer:=0;
    repeat
      repeat
        BufferA:=ansistring(Buffer);
        ReadSet.count:=1;
        ReadSet.Socket:=Socket;
        ErrorSet.count:=1;
        ErrorSet.Socket:=Socket;
        TimeVal.tv_sec:=0;
        TimeVal.tv_usec:=10000;
        R:=select(Socket+1, @ReadSet, nil, @ErrorSet, @TimeVal);
        if (R=SOCKET_ERROR) or (ErrorSet.count>0) then
        begin
          Log('[IRC] '+ConnectingFrom+' '+L_SELECT_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create(L_CONNECTION_ERROR+'.');
        end;

        if (ReadSet.count=0)or(R=0) then
          Break;  // nothing to read

        R:=ioctlsocket(Socket, FIONREAD, Bytes);
        if R=SOCKET_ERROR then
        begin
          Log('[IRC] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create(L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
        end;

        if Bytes=0 then  // software disconnect
        begin
          Log('[IRC] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' (Graceful disconnect).');
          raise Exception.Create('Connection reset by peer');
        end;

        if DataFloodCheck(Bytes) then
        begin
          RuptureError('Excess flood');
          raise Exception.Create('Excess flood');
        end;
        SetLength(SA, Bytes);
        R:=recv(Socket, SA[1], Bytes, 0);
        if(R=0)or(R=SOCKET_ERROR)then
        begin
          Log('[IRC] '+ConnectingFrom+' '+L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
          raise Exception.Create(L_CONNECTION_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+')');
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
        ExecuteCommand(S);
      end;

      Inc(PingTimer);
      if PingTimer=18000 then
        SendLn('PING :'+ServerHost);
      if PingTimer>=24000 then
      begin
        QuitMsg:='Ping timeout';
        AddSeen;
        if Registered and not Modes['i'] then
          Broadcast('QUIT :Ping timeout');
        Break;
      end;
      if not Registered then
      begin
        Inc(RegTimer);
        if RegTimer>=3000 then
        begin
          RuptureError('Registration timeout');
          Break;
        end;
      end;
    until Socket=0;
    Log('[IRC] '+L_CONNECTION_CLOSING_LINK+' '+ConnectingFrom);
    closesocket(Socket);

  except
    on E: Exception do
      begin
        if not Quit then
        begin
          QuitMsg:=E.Message;
          AddSeen;
          if Registered and not Modes['i'] then
            Broadcast('QUIT :'+QuitMsg);
          Log('[IRC] '+L_ERROR_WITH+' '+ConnectingFrom+' : '+E.Message);
        end;
      end;
    end;

  Sleep(50);
  CloseConnection;

  if Registered then
    if not SilentQuit then
      EventLog(Format(L_IRC_DISCONNECTED, [Nickname, ConnectingFrom, QuitMsg]))
    else
      EventLog(Format(L_IRC_DISCONNECTED_SILENT, [Nickname, ConnectingFrom]))
  else
  if not Authorized then
    EventLog(Format(L_IRC_DISCONNECTED_UNKAUTH, [ConnectingFrom]))
  else
    EventLog(Format(L_IRC_DISCONNECTED_UNKNOWN, [ConnectingFrom]));

  UserThreadList.Remove(Self);
  FreeOnTerminate:=True;
end;

procedure TUser.ExecuteCommand(S: string);
var Command: string;
begin
  S:=Copy(S, 1, 768);      
  Command:=UpperCase(StringSection(S, 0));
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
      ExecPass(S)
  else
    if Command='AUTHPONG' then
      ExecAuthpong(S)
  else
    if Command='QUIT' then
      ExecQuit(S)
  else
    if Command='AUTHPING' then
  else
    if Command='PONG' then
  else
    if Command='USERHOST' then
  else
    if Registered then
      if Command='PING' then
        ExecPing(S)
    else
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
      if Command='AWAY' then
        ExecAway(S)
    else
      if Command='TOPIC' then
        ExecTopic(S)
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
      if Command='SEEN' then
        ExecSeen(S)
    else
      if Command='MOTD' then
        ExecMotd(S)
    else
      if Command='LUSERS' then
        ExecLusers(S)
    else
      if Command='USERS' then
        ExecUsers(S)
    else
      if Command='INFO' then
        ExecInfo(S)
    else
      if Command='EXPECT' then
        ExecExpect(S)
    else
      if Command='ANNOUNCE' then
        ExecAnnounce(S)
    else
      if Command='CALC' then
        ExecCalc(S)
    else
      if Command='PRANK' then
        ExecPrank(S)
    else
      if Command='SENDFROM' then
        ExecSendfrom(S)
    else
      if Command='FORCEGAMEID' then
        ExecForcegameid(S)
    else
      if Command='ADDCHANNEL' then
        ExecAddchannel(S)
    else
      if Command='REMOVECHANNEL' then
        ExecRemovechannel(S)
    else
      if Command='RELOADSETTINGS' then
        ExecReload(S)
    else
      if Command='SHUTDOWN' then
        ExecShutdown(S)
    else
      if Command='BACK' then
        ExecAway('')
    else
      if (Command='KICKALL')or(Command='KILLALL') then
        ExecKickall(S)
    else
      if (Command='WHOIS')or(Command='IPLOOKUP') then
        ExecWhois(S)
    else
      if (Command='PRIVMSG') or (Command='NOTICE') then
        ExecMessage(Command,S)
    else
      if (Command='KICK') then
        ExecKick(S)
    else
      if (Command='KILL') then
        ExecKill(S)
    else
      if (Command='PERMABAN') or (Command='REMOVEBAN') then
        ExecBan(Command,S)
    else
      if (Command='OPER') or (Command='SOPER') or (Command='OWNER') or (Command='TAKEOWN') then
        ExecOper(Command,S)
    else
      if Command='MUTE' then
        ExecMute(true,S)
    else
      if Command='UNMUTE' then
        ExecMute(false,S)
    else
      if Command='TIME' then
        SendEvent(391, ServerHost+' :'+TextDateTime(Now))
    else
      SendError(421,Command)
  else
    if (Command<>'') and (Command<>'PING') then
      SendError(451,Command);
end;

procedure TUser.LogIn(S: String);
begin
  if InList(NickBanThreadList, Nickname) then
  begin
    RuptureError('You are banned.');
    EventLog(Format(L_IRC_HALT_BANNED_NICK, [Nickname, ConnectingFrom]));
    QuitMsg:='Killed by server: Banned nickname';
    Quit:=true;
    CloseConnection;
  end
  else
    if ForbiddenNick(Nickname) then
    begin
      RuptureError('Your nickname ('+Nickname+') is invalid, please change nickname and try again.');
      EventLog(Nickname+' ('+ConnectingFrom+') has been halted due to his nick being invalid.');
      QuitMsg:='Killed by server: Invalid nickname';
      Quit:=true;
      CloseConnection;
    end
  else
  begin
    EventLog(Format(L_IRC_LOGGED_IN, [Nickname, ConnectingFrom]));
    SendEvent(001, ':Welcome, '+Nickname+'!', false);
    SendEvent(002, ':Your host is '+ServerHost+', running MyWormNET version '+APPVERSION, false);
    SendEvent(003, ':This server was created '+StartupTime, false);
    SendEvent(004, ServerHost+' '+APPVERSION+' Qis'+IRCPrefModes+' Qtn'+IRCPrefModes, false);
    SendEvent(005, Format('PREFIX=(%s)%s STATUSMSG=%s CHANTYPES=%s NICKLEN=%u CHANNELLEN=%u QUITLEN=%u KICKLEN=%u NETWORK=%s CHANMODES=%s MODES=%s :are supported by this server',
      [IRCPrefModes, IRCPrefixes, IRCPrefixes, CHANTYPES, NICKLEN, CHANNELLEN, QUITLEN, KICKLEN, NetworkName, 'm,nt', '6']), false);
    Log('[IRC] > '+Format(L_IRC_LOG_WELCOME, [Nickname]));
    ExecLusers(S);
    ExecInfo(S);
    ExecMotd(S);
  end;
end;

function TUser.SendEvent(EventNo: Integer; S: String; Logging: Boolean=true): String;
var EventCode: String;
begin
  EventCode:=IntToStr(EventNo);
  while Length(EventCode) < 3 do
    EventCode:='0'+EventCode;
  Result:=':'+ServerHost+' '+EventCode+' '+Nickname+' '+S;
  SendLn(Result,Logging);
end;

function TUser.SendError(ErrNo: Integer; S: String; Logging: Boolean=true): String;
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

  Result:=SendEvent(ErrNo, S+' :'+StrOut, Logging);
end;
end;

function TUser.ServerMessage(S: String; MessageType: ShortInt=0): String;
var
  Sender: String;
begin
  case MessageType of
  0:
    Sender:='SERVER_MESSAGE';
  else
    Sender:='SERVER_GAMES';
  end;
    
  Result:=':'+Sender+'!root@'+ServerHost+' NOTICE '+Nickname+' :'+S;
  SendLn(Result);
end;

procedure TUser.ExecMute(Mute: Boolean; S: String);
var
  Side: Char;
  Pre, Command: String;
  User: TUser;
begin
  if Mute then Command:='MUTE'
  else Command:='UNMUTE';
  S:=StringSection(S, 0);
  if S <> '' then
  begin
    if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
    begin
      User:=LockUserByName(S);
      if User <> nil then
      begin
        if (Modes['q'])
          or ((Modes['a']) and not (User.Modes['q']))
          or ((Modes['o']) and not (User.Modes['a']) and not (User.Modes['q']))
          or ((Modes['h']) and not (User.Modes['a']) and not (User.Modes['q']) and not (User.Modes['o']))
        then
        begin
          if Mute then Side:='+'
          else Side:='-';

          if not User.ChangeMode(Side,'m',Self) then
          begin
            if Mute then Pre:='already'
            else Pre:='not';
            SendEvent(401, S+' :This user has '+Pre+' been muted');
          end;
        end
        else
          SendError(484,S);
      end
      else
        SendError(401,S);
      UserThreadList.UnlockList;
    end
    else
      SendError(481,Command);
  end
  else
    SendError(461,Command);
end;

procedure TUser.ExecKick(S: String);
const Command='KICK';
var
  Reason, Target: String;
  User: TUser;
  Channel: TChannel;
begin
  if S <> '' then
  begin
    Channel:=LockChannelByName(StringSection(S, 0));
    if Channel <> nil then
    begin
      Target:=StringSection(S, 1);
      if Pos(' ', S) <> 0 then
      begin
        Reason:=Copy(ContinuedSection(S, 2), 1, KICKLEN);
        CutLeadingColon(Reason);
        if Reason='' then
          Reason:='Bye.';
      end
      else
        Reason:='Bye.';

      if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
      begin
        User:=LockUserByName(Target);
        if User <> nil then
        begin
          if (Modes['q'])
            or ((Modes['a']) and not (User.Modes['q']))
            or ((Modes['o']) and not (User.Modes['a']) and not (User.Modes['q']))
          then
          begin
            Broadcast('KICK '+Channel.Name+' '+User.Nickname+' :'+Reason);
            User.ChannelsJoined.Remove(Channel);
            EventLog(Format(L_IRC_ACTION_KICK, [Nickname, User.Nickname, Channel.Name, Reason]));
          end
          else
            SendError(484,Target);
        end
        else
          SendError(401,Target);
        UserThreadList.UnlockList;
      end
      else
        SendError(481,Command);
    end
    else
      SendError(403,Command);
    ChannelThreadList.UnlockList;
  end
  else
    SendError(461,Command);
end;

procedure TUser.ExecKill(S: String);
const Command='KILL';
var
  Reason, Target: String;
  User: TUser;
begin
  if S <> '' then
  begin
    if ChannelExists(StringSection(S, 0)) then
      DeleteSections(S, 1);

    Target:=StringSection(S, 0);
    if Pos(' ', S) <> 0 then
    begin
      Reason:=Copy(ContinuedSection(S, 1), 1, KICKLEN);
      CutLeadingColon(Reason);
    end
    else
      Reason:='No reason specified';

    if (Modes['q'])or(Modes['a'])or(Modes['o']){or(Modes['h'])} then
    begin
      User:=LockUserByName(Target);
      if User <> nil then
      begin
        if (Modes['q'])
          or ((Modes['a']) and not (User.Modes['q']))
          or ((Modes['o']) and not (User.Modes['a']) and not (User.Modes['q']))
        then
        begin
          User.Kill(Self, Reason);
          EventLog(Format(L_IRC_ACTION_KILL, [Nickname, User.Nickname, Reason]));
        end
        else
          SendError(484,Target);
      end
      else
        SendError(401,Target);
      UserThreadList.UnlockList;
    end
    else
      SendError(481,Command);
    { begin
      Broadcast('QUIT :Killed: Attempted to use godly powers');
      SendLn('ERROR :Nice try, '+Nickname+'.');
      CloseConnection;
      end;
    }
  end
  else
    SendError(461,Command);
end;

procedure TUser.ExecWhois(S: String);
const Command='WHOIS';
var
  I: Integer;
  UserChannels, UserPrefixes, TrgRealname: String;
  User: TUser;
  ChannelList: TList;
  Channel: TChannel;
begin
  S:=StringSection(S, 0);
  UserChannels:='';
  UserPrefixes:='';
  User:=LockUserByName(S);
  if User <> nil then
  begin
    for I:=1 to Length(IRCPrefModes) do
      if User.Modes[IRCPrefModes[I]] then
        UserPrefixes:=UserPrefixes+IRCPrefixes[I];

    ChannelList:=ChannelThreadList.LockList;
    for I:=0 to ChannelList.Count-1 do
    begin
      Channel:=ChannelList[I];
      if User.InChannel(Channel) then
        UserChannels:=UserChannels+UserPrefixes+Channel.Name+' ';
    end;
    ChannelThreadList.UnlockList;

    TrgRealname:=User.Realname;
    if (Username='WWP') or (GameVersion.Valid and GameVersion.IsOlderThan('3.6.23.0')) then
      TrgRealname:=User.SafeRealname; // prevent WWP and old W:A from crashing

    SendEvent(311, User.Nickname+' '+User.Username+' '+StealthIP+' * :'+TrgRealname, false);
    if UserChannels<>'' then
      SendEvent(319, User.Nickname+' :'+UserChannels, false);
    SendEvent(312, User.Nickname+' '+ServerHost+' :'+NetworkName+' (MyWormNET '+APPVERSION+')', false);
    if (User = Self)
      or (((Modes['o']) or (Modes['a'])) and not (User.Modes['a']) and not (User.Modes['q']))
      or (Modes['q'])
    then
      SendEvent(338, User.Nickname+' '+User.ConnectingFrom+' :Actual IP', false);
    SendEvent(317, User.Nickname+' '+IntToStr(SecondsBetween(Now,User.LastMessageTime))+' '+IntToStr(User.SignonTime)+' :seconds idle, signon time', false);
  end;
  UserThreadList.UnlockList;
  SendEvent(318, S+' :End of /WHOIS list.', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecInfo(S: String);
const Command='INFO';
begin
  SendEvent(371, ':MyWormNET '+APPVERSION+' -- ', false);
  SendEvent(371, ':This is a custom WormNet IRC server emulator,', false);
  SendEvent(371, ':supporting custom set of IRC features.', false);
  SendEvent(371, ':The server software was written by', false);
  SendEvent(371, ':The_CyberShadow <thecybershadow@gmail.com>', false);
  SendEvent(371, ':and extended by StepS <github.com/StepS->', false);
  if WormNATPort>0 then
    SendEvent(371, ':[WormNATRouteOn:'+IntToStr(WormNATPort)+'] This server supports built-in WormNAT routing.', false);    
  SendEvent(371, ':This server was compiled '+CreationTime, false);
  SendEvent(371, ':This server is online since '+StartupTime, false);
  SendEvent(371, ':End of /INFO list.', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecMotd(S: String);
const Command='MOTD';
var
  Buf: String;
begin
  SendEvent(375, ':- '+NetworkName+' Message of the Day - ', false);
  S:=GetFile('motd.txt')+#13#10;
  while GetLine(S, Buf) do
    if(S<>'')or(Buf<>'') then
      SendEvent(372, ':- '+Buf, false);
  SendEvent(376, ':End of /MOTD command.', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecLusers(S: String);
const Command='LUSERS';
var UserCount: TUserCount;
begin
  UserCount:=GetFormattedUserCount;
  SendEvent(251, ':There are '+IntToStr(UserCount.Registered - UserCount.Invisible)+' users and '+IntToStr(UserCount.Invisible)+' invisible on this server', false);
  SendEvent(252, IntToStr(UserCount.Operators)+' :IRC Operators online', false);
  if UserCount.Unknown > 0 then
    SendEvent(253, IntToStr(UserCount.Unknown)+' :unknown connection(s)', false);
  SendEvent(254, IntToStr(GetChannelCount)+' :channels formed', false);
  SendEvent(255, ':I have '+IntToStr(UserCount.Registered)+' clients and 0 servers', false);
  ExecUsers(S);
  SendEvent(250, ':Highest connection count: '+IntToStr(MaxIRCUsers)+' ('+IntToStr(MaxIRCUsers)+' clients) ('+IntToStr(IRCConnections)+' connections received since last server (re)start)', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecUsers(S: String);
const Command='USERS';
var UserCount, MaxUsers: string;
begin
  UserCount:=IntToStr(GetRegisteredUserCount);
  MaxUsers:=IntToStr(MaxIRCUsers);
  SendEvent(265, ':Current local  users: '+UserCount+'  Max: '+MaxUsers, false);
  SendEvent(266, ':Current global users: '+UserCount+'  Max: '+MaxUsers, false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecSeen(S: String);
const Command='SEEN';
var
  I: Integer;
  TimeDiff, DaysAgo, HoursAgo, MinsAgo: Integer;
  LongAgo: String;
  SeenList: TList;
begin
  if SeenService then
  begin
    S:=StringSection(S, 0);
    if S<>'' then
    begin
      if not NickInUse(S) then
      begin
        SeenList:=SeenThreadList.LockList;
        if SeenList.Count < 1 then
          ServerMessage('Sorry, but there was no '+S+' around.')
        else
          for I:=0 to SeenList.Count-1 do
            with TSeen(SeenList[I]) do
              if TextMatch(S, Nick) then
              begin
                LongAgo:='';
                TimeDiff:=SecondsBetween(Now, LastSeen);
                DaysAgo:=TimeDiff div SecsPerDay;
                if DaysAgo > 0 then LongAgo:=LongAgo+IntToStr(DaysAgo)+' days ';
                HoursAgo:=(TimeDiff div 3600) mod 24;
                if HoursAgo > 0 then LongAgo:=LongAgo+IntToStr(HoursAgo)+' hours ';
                MinsAgo:=(TimeDiff div 60) mod 60;
                if MinsAgo > 0 then LongAgo:=LongAgo+IntToStr(MinsAgo)+' mins ';
                LongAgo:=LongAgo+IntToStr(TimeDiff mod 60)+' secs ';

                ServerMessage(Nick+' was last seen '+LongAgo+'ago, quit with a message: '+QuitMsg);
                Break;
              end
              else if I=SeenList.Count-1 then
                ServerMessage('Sorry, but there was no '+S+' around.');
        SeenThreadList.UnlockList;
      end
      else
        ServerMessage(S+' is online right now!');
    end
    else
      SendError(461,Command);
  end
  else
    ServerMessage('Sorry, but the seen service is disabled for this server.');
end;

procedure TUser.ExecAway(S: String);
const Command='AWAY';
begin
  S:=Copy(S,1,Pos(' ',S+' ')-1);
  if Pos(':',S) = 1 then Delete(S, 1, 1);
  S:=Copy(S, 1, 512);
  if (S='') and (Away) then
  begin
    Away:=false;
    SendEvent(305, ':You are no longer marked as being away');
    EventLog(Format(L_IRC_ACTION_BACK, [Nickname]));
  end
  else
    if (S<>'') then
    begin
      AwayMsg:=S;
      Away:=true;
      SendEvent(306, ':You have been marked as being away');
      EventLog(Format(L_IRC_ACTION_AWAY, [Nickname, AwayMsg]));
    end;
end;

procedure TUser.ExecTopic(S: String);
const Command='TOPIC';
var
  ChanName, NewTopic: string;
  Channel: TChannel;
begin
  ChanName:=StringSection(S, 0);
  if ChanName <> '' then
  begin
    DeleteSections(S, 1);
    Channel:=LockChannelByName(ChanName);
    if Channel <> nil then
    begin
      CutLeadingColon(S);
      NewTopic:=ContinuedSection(S, 0);
      if NewTopic<>'' then
      begin
        if (Modes['h']) or (Modes['o']) or (Modes['a']) or (Modes['q']) then
        begin
          Channel.Topic.Text:=NewTopic;
          Channel.Topic.SetBy:=Nickname;
          Channel.Topic.TimeSet:=DateTimeToUnix(Now);
          Broadcast('TOPIC '+Channel.Name+' :'+Channel.Topic.Text, Channel);
          EventLog(Format(L_IRC_ACTION_TOPIC, [Nickname, Channel.Name, Channel.Topic.Text]));
        end
        else
          SendError(481, Command);
      end
      else
      begin
        SendEvent(332, Channel.Name+' :'+Channel.Topic.Text, false);
        SendEvent(333, Channel.Name+' '+Channel.Topic.SetBy+' '+IntToStr(Channel.Topic.TimeSet), false);
      end;
    end
    else
      SendError(403,ChanName);
    ChannelThreadList.UnlockList;
  end
  else
    SendError(461,Command);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecForcegameid(S: string);
const Command='FORCEGAMEID';
begin
  S:=StringSection(S, 0);
  if (Modes['a']) or (Modes['q']) then
  begin
    if S<>'' then
    begin
      InterLockedExchange(GameCounter, StrToIntDef(S, 1000000));
      EventLog(Format(L_IRC_ACTION_FORCEGAMEID, [Nickname,S]));
      ServerMessage('Ok. Game counter is now: '+IntToStr(GameCounter));
    end
    else
      SendError(461,Command);
  end
  else
    SendError(481, Command);
end;

procedure TUser.ExecAddchannel(S: string);
const Command='ADDCHANNEL';
var
  Name, Scheme, Topic: String;
begin
  if (Modes['q']) or (Modes['a']) then
  begin
    Trim(S);
    Name:=StringSection(S, 0);
    if (Name<>'') and (Name<>'#') then
    begin
      Scheme:=StringSection(S, 1);
      Topic:=ContinuedSection(S, 2);
      if Name[1] <> '#' then Name:='#'+Name;
      if Scheme = '' then Scheme:='Pf,Be';
      if Topic = '' then Topic:='00 New channel';

      EventLog(Format(L_IRC_ACTION_ADDCHANNEL, [Nickname, Name]));
      if AddChannel(Name, Scheme, Topic, Nickname) then
      begin
        ExecJoin(Name);
        ServerMessage('Channel '+Name+' has been created.');
      end
      else
        ServerMessage('Channel '+Name+' has already been created.');
    end
    else
      SendError(461, Command);
  end
  else
    SendError(481, Command);
end;

procedure TUser.ExecRemovechannel(S: string);
const Command='REMOVECHANNEL';
var
  Channel: TChannel;
begin
  S:=StringSection(S, 0);
  if (Modes['q']) or (Modes['a']) then
  begin
    if S<>'' then
    begin
      Channel:=LockChannelByName(S);
      if Channel <> nil then
      begin
        EventLog(Format(L_IRC_ACTION_REMOVECHANNEL, [Nickname, Channel.Name]));
        if RemoveChannel(Channel) then
          ServerMessage('Channel '+S+' has been removed.')
      end
      else
        SendError(403, S);
      ChannelThreadList.UnlockList;
    end
    else
      SendError(461, Command);
  end
  else
    SendError(481, Command);
end; 
procedure TUser.ExecReload(S: string);
const Command='RELOADSETTINGS';
begin
  if Modes['q'] then
  begin
    ServerMessage('Reloading the server configuration...');
    LoadParams;
    ServerMessage('Configuration reloaded.');
    EventLog(Format(L_IRC_ACTION_RELOADSETTINGS, [Nickname]));
  end
  else
    SendError(481, Command);
end;

procedure TUser.ExecShutdown(S: string);
const Command='SHUTDOWN';
begin
  if Modes['q'] then
  begin
    ExecAnnounce('The server is shutting down!');
    EventLog(Format(L_IRC_ACTION_SHUTDOWN, [Nickname]));
    Sleep(1000);
    Halt(0);
  end
  else
    SendError(481, Command);
end;
procedure TUser.ExecBan(Command, S: String);
var
  B, P: Boolean;
  BanFile: String;
  Target, Reason, BType, LBType, Result, LResult: String;
  User: TUser;
begin
  if (Modes['o']) or (Modes['a']) or (Modes['q']) then
  begin
    if S<>'' then
    begin
      B:=true;
      P:=true;
      if Pos(' ', S) <> 0 then
      begin
        Target:=StringSection(S, 0);
        Reason:=Copy(S, Pos(' ', S)+1, KICKLEN);
        CutLeadingColon(Reason);
      end
      else
      begin
        Target:=S;
        Reason:='No reason specified';
      end;

      if Command='PERMABAN' then
      begin
        LResult:=L_IRC_ACTION_BAN;
        if (Pos('.',Target) = 0) and (Pos(':',Target) = 0) then
        begin
          BanFile:='banlist_nicks.txt';
          BType:='nickname';
          LBType:=L_IRC_TYPE_NICKNAME;
          Result:='banned';
          if InList(NickBanThreadList, Target) then
            B:=false;

          if not Modes['q'] then
            if SuperNameExists(Target) then
            begin
              B := false;
              P := false;
            end;
          if (B) then
            NickBanThreadList.Add(NewListEntry(Target, Nickname, Reason, true, Now, -1));
        end
        else
        begin
          BanFile:='banlist_ips.txt';
          BType:='IP';
          LBType:=L_IRC_TYPE_IP;
          Result:='permabanned';
          if InList(IPBanThreadList, Target) then
            B:=false;
          if not Modes['q'] then
            if SuperIPExists(Target) then
            begin
              B := false;
              P := false;
            end;
          if (B) then
            IPBanThreadList.Add(NewListEntry(Target, Nickname, Reason, true, Now, -1));
        end;

        if B then
        begin
          TextToFile(Target, BanFile, true);
          if BType='nickname' then
          begin
            User:=LockUserByName(Target);
            if User <> nil then
              User.Kill(Self, 'Banned: '+Reason);
            UserThreadList.UnlockList;
          end
          else
          begin
            User:=LockUserByIP(Target);
            if User <> nil then
              User.Kill(Self, 'Permabanned: '+Reason);
            UserThreadList.UnlockList;
          end;
        end;
      end
      else
      begin
        LResult:=L_IRC_ACTION_UNBAN;
        Result:='unbanned';
        if (Pos('.',Target) = 0) and (Pos(':',Target) = 0) then
        begin
          BType:='nickname';
          LBType:=L_IRC_TYPE_NICKNAME;
          if InList(NickBanThreadList, Target) then
            RemoveListEntry(NickBanThreadList, Target)
          else B:=false;
        end
        else
        begin
          BType:='IP';
          LBType:=L_IRC_TYPE_IP;
          if InList(IPBanThreadList, Target) then
            RemoveListEntry(IPBanThreadList, Target)
          else B:=false;
        end;

        if B then
        begin
          if BType='nickname' then
            UpdateListInFile(NickBanThreadList, BanFile)
          else
            UpdateListInFile(IPBanThreadList, BanFile)
        end;
      end;

      if B then
      begin
        EventLog(Format(LResult, [Nickname, LBType+' "'+Target+'"', Reason]));
        ServerMessage(UpperFCStr(BType)+' "'+Target+'" has been '+Result+'.')
      end
      else
        if P then
          ServerMessage(UpperFCStr(BType)+' '+Target+' has already been '+Result+'.')
        else
        begin
          if BType='nickname' then
          begin
            User:=LockUserByName(Target);
            if User <> nil then
              User.ServerMessage(Nickname+' has just attempted to ban you by nick!');
            UserThreadList.UnlockList;
          end
          else
          begin
            User:=LockUserByIP(Target);
            if User <> nil then
              User.ServerMessage(Nickname+' has just attempted to ban you by IP!');
            UserThreadList.UnlockList;
          end;

          EventLog(Format(L_IRC_ACTION_FAILBAN, [Nickname, LBType+' "'+Target+'"']));
        end;
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
  Description, Target: String;
  User: TUser;
begin
  if Pos(' ', S) <> 0 then
  begin
    Target:=StringSection(S, 0);
    Description:=ContinuedSection(S, 1);
  end
  else
  begin
    Target:=S;
    Description:='Something bad happened.';
  end;

  if (Modes['q'])or(Modes['a'])or(Modes['o']) then
  begin
    User:=LockUserByName(Target);
    if User <> nil then
      begin
        if (Modes['q'])
        or ((Modes['a']) and not (User.Modes['q']))
        or ((Modes['o']) and not (User.Modes['a']) and not (User.Modes['q']))
        then
          begin
            User.LastSenior:=Nickname;
            User.SendLn('ERROR :'+Description);
            EventLog(Format(L_IRC_ACTION_PRANK, [Nickname, User.Nickname, Description]));
          end
        else
          SendError(484,Target);
      end
      else
        SendError(401,Target);
    UserThreadList.UnlockList;
  end;
end;

procedure TUser.ExecKickall(S: String);
const Command='KICKALL';
var
  I: Integer;
  UserList: TList;
  User: TUser;
begin
  if Modes['q'] then
  begin
    UserList := UserThreadList.LockList;
    for I:=0 to UserList.Count-1 do
    begin
      User := UserList[I];
      if User.Modes['q'] = false then
        User.Kill(Self, 'Massive killing started by '+Nickname);
    end;
    UserThreadList.UnlockList;
    EventLog(Format(L_IRC_ACTION_KICKALL, [Nickname]));
  end
  else
    SendError(481,Command);
end;

procedure TUser.ExecSendfrom(S: String);
const Command='SENDFROM';
var
  Target, HisCommand: String;
  User: TUser;
begin
  if (Modes['q']) then
    if (S='') then
      SendError(461,Command)
    else
    begin
      Target:=StringSection(S, 0);
      HisCommand:=ContinuedSection(S, 1);
      while Pos(' ',HisCommand) = 1 do
        Delete(HisCommand, 1, 1);
      if HisCommand <> '' then
      begin
        User:=LockUserByName(Target);
        if User <> nil then
        begin
          Target:=User.Nickname;
          EventLog(Nickname+' has executed a command on behalf of '+Target+': '+HisCommand);
          User.ExecuteCommand(HisCommand);
        end;
        UserThreadList.UnlockList;
      end
      else
        SendError(461,Command);
    end
  else
    SendError(481,Command);
end;

procedure TUser.ExecAnnounce(S: String);
const Command='ANNOUNCE';
var
  I: Integer;
  ChannelList: TList;
  Channel: TChannel;
begin
  if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
  begin
    if S <> '' then
    begin
      ChannelList:=ChannelThreadList.LockList;
      for I:=0 to ChannelList.Count-1 do
      begin
        Channel:=ChannelList[I];
        Broadcast(':SERVER_ANNOUNCEMENT!root@'+ServerHost+' NOTICE '+Channel.Name+' :'+Nickname+': '+S, Channel, true);
      end;
      ChannelThreadList.UnlockList;
      EventLog(Format(L_IRC_ACTION_ANNOUNCE, [Nickname, S]));
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
  IsonBuff, Target: String;
begin
  IsonBuff:='';
  while Pos(' ',S) <> 0 do
  begin
    Target:=StringSection(S, 0);
    DeleteSections(S, 1);
    if NickInUse(Target) then
      IsonBuff:=IsonBuff+Target+' ';
  end;
  if S='' then
    SendEvent(303, ':'+IsonBuff)
  else
  begin
    Target:=S;
    if NickInUse(Target) then
      IsonBuff:=IsonBuff+Target+' ';
    SendEvent(303, ':'+IsonBuff);
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

procedure TUser.ExecCalc(S: String);
const Command='CALC';
var
  Operation, FullParam: string;
  //Params: TStringList;
begin
  S:=Copy(S, 1, 512);
  if (Modes['v']) or (Modes['h']) or (Modes['o']) or (Modes['a']) or (Modes['q']) then
  begin
    if S<>'' then
    begin
      Operation:=LowerCase(StringSection(S, 0));
      DeleteSections(S, 1);
      if Operation <> '' then
      begin
        FullParam:=S;
        {S:=S+' ';
        Params := TStringList.Create;
        while Pos(' ',S) <> 0 do
        begin
          Params.Add(StringSection(S, 0));
          DeleteSections(S, 1);
        end;}
        if Operation = 'help' then
        begin
          ServerMessage('Supported calc commands:');
          ServerMessage('decode64 <string>: will decode string from base64');
          ServerMessage('encode64 <string>: will encode string to base64');
          ServerMessage('cp-wa-1251 <string>: will convert a WA string into a Win-1251 string');
        end
        else
        begin
          if FullParam <> '' then
          begin
            if Operation = 'decode64'        then ServerMessage('Decode64 result: '+StrDecode64(FullParam))
            else if Operation = 'encode64'   then ServerMessage('Encode64 result: '+StrEncode64(FullParam))
            else if Operation = 'strtohex'   then ServerMessage('StrToHex result: '+StrToHex(FullParam))
            else if Operation = 'cp-wa-1251' then ServerMessage('Converted W:A to Win1251: '+CP_WAto1251(FullParam))
            else ServerMessage('Sorry, but I''m not sure what you''re referring to. Use calc with the help parameter to see the full list.');
          end
          else
            ServerMessage('Sorry, but you didn''t specify anough parameters to process your operation...');
        end;
        //Params.Destroy;
      end
      else
        SendError(461,Command);
    end
    else
      SendError(461,Command);
  end
  else
    SendError(481,Command);
end;

procedure TUser.ExecPass(S: String);
const Command='PASS';
begin
  if UserPass='' then
  begin
    UserPass:=S;
    if not Authorized then
    begin
      SendLn('ERROR :Bad password');
      if (Socket <> 0) then closesocket(Socket); Socket:=0;
    end;
  end
  else
    SendError(462,Command);
end;

procedure TUser.ExecNick(S: String);
const Command='NICK';
var
  I, K: Integer;
  ChannelsJoinedList: TList;
  Channel: TChannel;
begin
  if not Authorized then
  begin
    RuptureError('Bad password');
    CloseConnection;
  end
  else
  begin
    S:=Copy(SanitizeName(StringSection(S, 0)), 1, NICKLEN);
    if TempNick='' then
    begin
      if (S='') or (Pos(S[1], Numbers) <> 0) then
        SendEvent(432, S+' :Erroneous nickname')
      else
      begin
        if NickInUse(S) then
          SendEvent(433, S+' '+S+' :Nickname is already in use')
        else if not ValidateRelativity(WhiteNickThreadList, S, ConnectingFrom) then
          ServerKill('Sorry, nickname '+S+' is protected. Please choose another one.')
        else if not ValidateRelativity(WhitePassauthThreadList, S, UserPass) then
          ServerKill('Bad password for nickname '+S)                                    //two factor auth
        else begin
          Nickname:=S;
          TempNick:=Nickname;
          if TempUsername<>'' then
            LogIn(S);
        end;
      end;
    end
    else
    begin
      if not ((Modes['o']) or (Modes['a']) or (Modes['q'])) then
      begin
        SendError(400,S);
        EventLog(Format(L_IRC_ACTION_NICK_FAIL, [Nickname, S]));
      end
      else
      begin
        if (S='') or (Pos(S[1], Numbers) <> 0) then
          SendEvent(432, S+' :Erroneous nickname')
        else if NickInUse(S) then
          SendEvent(433, S+' :Nickname is already in use')
        else if not ValidateRelativity(WhiteNickThreadList, S, ConnectingFrom) then
          SendEvent(432, S+' :Sorry, nickname '+S+' is protected. Please choose another one.')
        else if not ValidateRelativity(WhitePassauthThreadList, S, UserPass) then
          ServerKill('Bad password for nickname '+S)
        else
        begin
          Broadcast('QUIT :Changing nick to '+S, false, true, true);
          ChannelsJoinedList:=ChannelsJoined.LockList;
          SendLn(Self, 'NICK :'+S);
          EventLog(Format(L_IRC_ACTION_NICK_SUCCESS, [Nickname, S]));
          Nickname:=S;
          for I := 0 to ChannelsJoinedList.Count-1 do
          begin
            Channel:=TChannel(ChannelsJoinedList[I]);
            Broadcast('JOIN '+Channel.Name, Channel, false, true, true);
            for K:=1 to Length(IRCPrefModes) do
              if Modes[IRCPrefModes[K]] then
                Broadcast(':'+ServerHost+' MODE '+Channel.Name+' +'+IRCPrefModes[K]+' '+Nickname, Channel, true, true, true);
          end;
          ChannelsJoined.UnlockList;
        end;
       end;
    end;
  end;
end;

procedure TUser.ExecUser;
const Command='USER';
begin
  if not Authorized then
  begin
    RuptureError('Bad password');
    CloseConnection;
  end
  else
  begin
    if (TempUsername='') or (Modes['q']) then
    begin
      if UserPass = IRCPassword+' ' then
        Username:='Username'
      else if UserPass = IRCPassword then
        if Pos('WWP hostname servername :', S) = 1 then
          Username:='WWP'
        else begin 
          Username:='~'+SanitizeName(StringSection(S, 0));
          if Username='~' then Username:='~UserName'
        end
      else begin
        Username:=SanitizeName(StringSection(S, 0));
        if Username='' then Username:='Registered';
      end;
      TempUsername:=Username;
      Hostname:=StringSection(S, 1);
      Servername:=StringSection(S, 2);
      Realname:=TrimLeft(GetTrail(S));
      GetSafeRealname;
      GetGameVersion;
      
      if GameVersion.Valid and GameVersion.IsOlderThan(MinimumVersion) then
        ServerKill('Sorry, your version of the game ('+GameVersion.Str+') is too old. Please update via http://wa.team17.com')
      else if TempNick<>'' then
        LogIn(S);
    end
    else
      SendError(462,Command)
    end
end;

procedure TUser.ExecQuit(S: String);
const Command='QUIT';
begin
// :CyberShadow!cybershado@38F7DF98.502358C0.F6DD7E74.IP QUIT :Input/output error
  if not Quit then
  begin
    QuitMsg:=S;
    if Pos(':',QuitMsg) = 1 then
      Delete(QuitMsg, 1, 1);
    QuitMsg:=Copy(QuitMsg, 1, QUITLEN);
    if (QuitMsg='') or (QuitMsg=' ') then
      QuitMsg:='Quit'
    else if not ((QuitMsg='Joined Game') or (Pos('Hosting a game: ', QuitMsg) <> 0)) then
      QuitMsg:='Quit: '+QuitMsg;
    if not (Modes['i']) and Registered then
      Broadcast('QUIT :'+QuitMsg);
    Quit:=true;
    AddSeen;
    CloseConnection;
  end;
end;

procedure TUser.ExecNames(S: String);
const Command='NAMES';
var
  I, K: Integer;
  StrOut: String;
  Channel: TChannel;
  UserList: TList;
  User: TUser;
begin
  S:=StringSection(S, 0);
  Channel:=LockChannelByName(S);
  if Channel <> nil then
  begin
    S:=Channel.Name;
    StrOut:='= '+S+' :';
    UserList:=UserThreadList.LockList;
    for I:=0 to UserList.Count-1 do
    begin
      User:=UserList[I];
      if (User.InChannel(Channel)) and not (User.Modes['i']) then
      begin
        for K:=1 to Length(IRCPrefModes) do
          if User.Modes[IRCPrefModes[K]] then
            StrOut:=StrOut+IRCPrefixes[K];
        StrOut:=StrOut+User.Nickname+' ';
      end;
    end;
    UserThreadList.UnlockList;
    SendEvent(353, StrOut, false);
  end;
  ChannelThreadList.UnlockList;
  SendEvent(366, S+' :End of /NAMES list.', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecJoin(S: String);
const Command='JOIN';
var
  K: Integer;
  CurChan: String;
  Channel: TChannel;
begin
  S:=S+',';
  repeat
  begin
    CurChan:=Copy(S,1,Pos(',',S)-1);
    Delete(S,1,Pos(',',S));
    Channel:=LockChannelByName(CurChan);
    if Channel <> nil then
    begin
      if not InChannel(Channel) then
      begin
        Quit:=false;
        CurChan:=Channel.Name;
        EventLog(Format(L_IRC_JOIN, [Nickname, CurChan]));
        ChannelsJoined.Add(Channel);
        //:CyberShadow-MD!Username@no.address.for.you JOIN :#AnythingGoes
        if not Modes['i'] then
        begin
          Broadcast('JOIN :'+CurChan, Channel);
          for K:=1 to Length(IRCPrefModes) do
            if Modes[IRCPrefModes[K]] then
              Broadcast(':'+ServerHost+' MODE '+CurChan+' +'+IRCPrefModes[K]+' '+Nickname, Channel, true);
        end
        else
          SendLn(Self, 'JOIN :'+CurChan);
        ExecTopic(CurChan);
        ExecNames(CurChan);
      end
    end
    else
      SendError(403,CurChan);
    ChannelThreadList.UnlockList;
  end
  until Pos(',',S) = 0;
end;

procedure TUser.ExecPart(S: String);
const Command='PART';
var
   Chan, PartMsg, Appendix: String;
   Channel: TChannel;
begin
  if S<>'' then
  begin
    Chan:=StringSection(S, 0);
    PartMsg:=Copy(GetTrail(S), 1, 160);
    Channel:=LockChannelByName(Chan);
    if Channel <> nil then
    begin
      Chan:=Channel.Name;
      if InChannel(Channel) then
      begin
        if PartMsg <> '' then
          Appendix:=' :'+PartMsg
        else
          Appendix:='';
        if not Quit then
          if Modes['i'] then
            SendLn('PART '+Chan+Appendix)
          else
            Broadcast('PART '+Chan+Appendix, Channel);
        ChannelsJoined.Remove(Channel);
        if PartMsg <> '' then
          EventLog(Format(L_IRC_PART_EX, [Nickname, Chan, PartMsg]))
        else
          EventLog(Format(L_IRC_PART, [Nickname, Chan]));
      end;
    end
    else
      SendError(403, Chan);
    ChannelThreadList.UnlockList;
  end
//  else
//    SendError(461, Command);   //Commenting to mitigate W:A's kick handling
end;

procedure TUser.ExecMode(S: String);
const Command='MODE';
var
  I, K: Integer;
  Side, Mode: Char;
  ModeStr, Target: String;
  Channel: TChannel;
  UserList: TList;
  User: TUser;
begin
  Target:=StringSection(S, 0);
  ModeStr:='';
  Channel:=LockChannelByName(Target);
  if Channel <> nil then
  begin
    Delete(S,1,Pos(Channel.Name,S));
    if Pos(' ', S) <> 0 then
    begin
      DeleteSections(S, 1);
      if (Pos('+',S))or(Pos('-',S)) = 1 then
      begin
        if Pos(' ', S) <> 0 then
        begin
          if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
          begin
            ModeStr:=Copy(S, 1, Pos(' ', S));
            Delete(S, 1, Pos(' ', S));
            Target:=S;
            User:=LockUserByName(Target);
            if User <> nil then
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
                  or (not (Mode='v') and not (Mode='m') and ((Modes['h'])
                     and not (Modes['o']) and not (Modes['a']) and not (Modes['q'])))
                  or ((Mode='s') and (Target<>Nickname))
                  or ((User.Modes['q']) and not (Modes['q']))
                  or ((User.Modes['a']) and not (Modes['q']))
                  or ((User.Modes['o']) and not (Modes['o']) and not (Modes['a']) and not (Modes['q']))
                  )
                  then
                    User.ChangeMode(Side,Mode,Self)
                  else
                    if ((Mode='s') and (Target<>Nickname)) then
                      SendError(502,Command)
                    else
                    begin
                      SendError(482,Command);
                      Break;
                    end;
                end;
              end;
            end
            else
              SendError(401,Target);
            UserThreadList.UnlockList;
          end
          else
            SendError(481,Command);
        end
        else
          if Pos('+Q',S) <> 0 then
          begin
            UserList:=UserThreadList.LockList;
            for I:=0 to UserList.Count-1 do
            begin
              User:=UserList[I];
              if User.Modes['m'] then
                SendEvent(367, Channel.Name+' '+User.Nickname+'!'+User.Username+'@'+StealthIP+' '+User.LastSenior+' '+IntToStr(User.LastBanTime), false);
            end;
            UserThreadList.UnlockList;
            SendEvent(368, Channel.Name+' :End of Muted Users List', false);
            Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
          end;
      end
      else
      begin
        SendEvent(324, Channel.Name+' +tn', false);
        SendEvent(329, Channel.Name+' '+IntToStr(Channel.Topic.TimeSet), false);
        Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
      end;
    end
    else
    begin
      SendEvent(324, Channel.Name+' +tn', false);
      SendEvent(329, Channel.Name+' '+IntToStr(Channel.Topic.TimeSet), false);
      Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
    end;
  end
  else
    SendError(403,Target);
  ChannelThreadList.UnlockList;
end;

procedure TUser.ExecMessage(Command, S: String);
var
  K: Integer;
  Target, Msg, WACMsg, Subcommand, Substr: String;
  User: TUser;
  Channel: TChannel;
begin
  Target:=StringSection(S, 0);
  Target:=Copy(Target, 1, 20);
  Msg:=GetTrail(S);
  if Length(Msg) > 512 then
  begin
    ServerKill('Message too long');
    EventLog(Nickname+' has been killed due to exceeding the maximum message length. Here''s part of his message: "'+CP_WAto1251(Copy(Msg,1,512))+'".');
  end
  else
  begin
    Trim(Msg);
    WACMsg:=CP_WAto1251(Msg);
    if MessageFloodCheck(Length(Msg)) then
    begin
      ServerKill('Message flood');
      EventLog(Nickname+' has been killed due to message flood. Last message: "'+WACMsg+'".');
    end
    else
    begin
      if Msg <> '' then
      begin
        if not ContextCommand(Msg) then
        begin
          if Modes['m'] then
            ServerMessage('Sorry, but you are muted by '+LastSenior+' and thus cannot talk.')
          else
          begin
            Channel:=LockChannelByName(Target);
            if Channel <> nil then
            begin
              if InChannel(Channel) then
              begin
                Target:=Channel.Name;

                Substr:=Msg;

                if Pos('!',Substr) = 1 then
                begin
                  Subcommand:=Copy(Substr, 2, Pos(' ',Substr+' ')-2);
                  DeleteSections(Substr, 1);

                  if ((TextMatch(Subcommand,'host')) or (TextMatch(Subcommand,'phost')) and not (NickInUse('HostingBuddy'))) then
                  begin
                    EventLog('[COMMAND] <'+Nickname+'> '+Msg);
                    ServerMessage('Sorry, but there is no HostingBuddy on this server. Try hosting yourself or install WormNAT2 if you can''t: http://worms2d.info/WormNAT2')
                  end
                  else
                  begin
                    if SeenService and TextMatch(Subcommand,'seen') then
                      ExecSeen(Substr);

                    EventLog('['+Target+'] <'+Nickname+'> '+WACMsg);
                    Broadcast(Command+' '+Target+' :'+Msg, Channel, false, true);
                  end;
                end
                else
                begin
                  EventLog('['+Target+'] <'+Nickname+'> '+WACMsg);
                  Broadcast(Command+' '+Target+' :'+Msg, Channel, false, true);
                end;
              end;
            end
            else
            begin
              for K:=1 to Length(IRCPrefixes) do
                if Pos(IRCPrefixes[K],Target) <> 0 then
                  Delete(Target,Pos(IRCPrefixes[K],Target),1);
              User:=LockUserByName(Target);
              if User=nil then
                SendError(401,Target)
              else
              begin
                if User.Registered then
                begin
                  Target := User.Nickname;
                  if User.Away then
                    SendEvent(301,Target+' :'+User.AwayMsg);
                  EventLog('['+Command+'] <'+Nickname+'> -> <'+Target+'> '+WACMsg);
                  User.SendLn(Self, Command+' '+Target+' :'+Msg, false);
                end
                else
                  SendError(401,Target);
              end;
              UserThreadList.UnlockList;
            end;
            ChannelThreadList.UnlockList;
        //  Sleep(1000); // throttle
          end;
        end;
      end
      else
        SendError(412,Command);
    end;
  end;
end;

procedure TUser.ExecOper(Command, S: String);
var
  I: Integer;
  Mode: Char;
  Description, AdminType: String;
  ChannelList: TList;
  Channel: TChannel;
begin
  if StringSection(S, 0)<>IRCOperPassword then
    DeleteSections(S, 1);  // ignore username
  if S=IRCOperPassword then
  begin
    if Command='OPER' then
    begin
      Description:=L_IRC_ADMIN_OPER;
      AdminType:='Operator';
      Mode:='o';
    end
    else
    begin
      Description:=L_IRC_ADMIN_OWNER;
      AdminType:='Owner';
      Mode:='q';
      if not Modes['q'] then
        TSuper.Create(Nickname, ConnectingFrom);
    end;
    if not Modes[Mode] then
    begin
      EventLog(Format(L_IRC_ADMIN_LOGIN, [Nickname, Description]));
      Modes[Mode]:=True;
      if not Modes['i'] then
      begin
        ChannelList:=ChannelThreadList.LockList;
        for I:=0 to ChannelList.Count-1 do
        begin
          Channel:=ChannelList[I];
          Broadcast(':'+ServerHost+' MODE '+Channel.Name+' +'+Mode+' '+Nickname, Channel, true);
        end;
        ChannelThreadList.UnlockList;
      end;
      SendEvent(381,':You are now an IRC '+AdminType);
    end;
  end
  else
    SendError(464,Command);
end;

procedure TUser.ExecWho(S: String);
const Command='WHO';
var
  I, K: Integer;
  Pref, Target, ChanStr, TrgRealname, TrgAddress: String;
  IsChannelRequest: Boolean;
  Presence: char;
  UserList, ChannelsJoinedList: TList;
  User: TUser;
  Channel: TChannel;
begin
  //:wormnet1.team17.com 352 DoctorWho #AnythingGoes Username no.address.for.you wormnet1.team17.com TiCPU H :0 TiCpu
  //:wormnet1.team17.com 315 DoctorWho * :End of /WHO list.
  IsChannelRequest:=ChannelExists(S);
  UserList:=UserThreadList.LockList;
  for I:=0 to UserList.Count-1 do
  begin
    User:=UserList[I];
    if User.Away then
      Presence:='G'
    else
      Presence:='H';
    Pref:='';

    for K:=1 to Length(IRCPrefModes) do
      if User.Modes[IRCPrefModes[K]] then
      begin
        Pref:=IRCPrefixes[K];
        Break;
      end;

    if S<>'' then
      Target:=S
    else
      Target:=User.Nickname;

    TrgRealname:=User.Realname;
    if Username = 'WWP' then     // prevent WWP from crashing
      TrgRealname:=User.SafeRealname;

    TrgAddress:=StealthIP;
    if (Modes['o']) or (Modes['a']) or (Modes['q']) then
      TrgAddress:=User.ConnectingFrom;

    if not IsChannelRequest then
    begin
      for K:=1 to Length(IRCPrefixes) do
        if Pos(IRCPrefixes[K],S) <> 0 then
          Delete(S,Pos(IRCPrefixes[K],S),1);

      if (S<>'') and (S<>User.Nickname) then continue;

      ChanStr:='*';

      ChannelsJoinedList:=ChannelsJoined.LockList;
      if ChannelsJoinedList.Count > 0 then
        ChanStr:=TChannel(ChannelsJoinedList[0]).Name;
      ChannelsJoined.UnlockList;

      if not TextMatch(User.Nickname, Nickname) then
      begin
        if not (User.Modes['i']) then
          if User.Registered then
            SendEvent(352, ChanStr+' '+User.Username+' '+TrgAddress+' '+ServerHost+' '+Target+' '+Presence+Pref+' :0 '+TrgRealname, false);
      end
      else
        SendEvent(352, ChanStr+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Target+' '+Presence+Pref+' :0 '+TrgRealname, false);
      if (S<>'') and (TextMatch(S,User.Nickname)) then Break;
    end
    else
    begin
      Channel:=LockChannelByName(S);
      if Channel <> nil then
        if User.InChannel(Channel) then
        begin
          if not (User.Modes['i']) and not ((Pos(' o ',S+' ') <> 0) and not ((User.Modes['h']) or (User.Modes['o']) or (User.Modes['a']) or (User.Modes['q']))) then
            if not TextMatch(User.Nickname, Nickname) then
              SendEvent(352, Channel.Name+' '+User.Username+' '+TrgAddress+' '+ServerHost+' '+User.Nickname+' '+Presence+Pref+' :0 '+TrgRealname, false)
            else
              SendEvent(352, Channel.Name+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Nickname+' '+Presence+Pref+' :0 '+TrgRealname, false);
        end;
      ChannelThreadList.UnlockList;
      if Channel = nil then
      begin
        EventLog('[ACHIEVEMENT GET]: WHO-smasher!');  //Go!
        Break;
      end;
    end;
  end;
  UserThreadList.UnlockList;
  if S='' then
    SendEvent(315, '* :End of /WHO list.', false)
  else
    SendEvent(315, S+' :End of /WHO list.', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecList(S: String);
const Command='LIST';
var I, J, N: Integer;
begin                     
  SendEvent(321, 'Channel :Users  Name', false);
  for I:=0 to Length(Channels)-1 do
  begin
    N:=0;
    for J:=0 to Length(Users)-1 do
      if Users[J].InChannel[Channels[I].Number] then
        Inc(N);
    SendEvent(322, Channels[I].Name+' '+IntToStr(N)+' :'+Channels[I].Topic, false);
  end;
  SendEvent(323, ':End of /LIST', false);
  Log('[IRC] > '+Format(L_IRC_LOG_RESPONSE, [Command, Nickname]));
end;

procedure TUser.ExecExpect(S: String);
const Command='EXPECT';
var
  I: Integer;
  User: TUser;
begin
  S:=Copy(S, 1, Pos(' ',S+' ')-1);
  Log(Format(L_IRC_EXPECT, [ConnectingFrom, S]));
  User:=UserByName(S);
  if User=nil then
    SendError(401,S)
  else
  begin
    SendLn(':'+ServerHost+' NOTICE '+Nickname+' :OK, expecting '+User.Nickname+' from his IP');
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

procedure TUser.ExecAuthpong(S: string);
begin
  if ForceAuthping then
    if S<>'' then
    begin
      TextToFile('/AUTHPONG '+S,ExtractFilePath(ParamStr(0))+'authpong.txt',true);
      SendLn('ERROR :Written to authpong.txt! '+S);
      Sleep(1000);
      closesocket(Socket);
      Socket:=0;
    end;
end;

procedure TUser.SendLn(S: string; Logging: Boolean=true);
var
  TStr: String;
  AStr: AnsiString;
begin
  if Socket=0 then Exit;
  if Logging then
    Log('[IRC] > '+S);
  AStr:=AnsiString(S);
  AStr:=AStr+#13#10;
  if send(Socket, AStr[1], Length(AStr), 0)<>Length(AStr) then
  begin
    Socket:=0;  // avoid infinite recursion
    Log('[IRC] > '+L_FAILED+' ('+WinSockErrorCodeStr(WSAGetLastError)+')');
  end;
end;

procedure TUser.Broadcast(S: string; Channel: TChannel; ExceptSelf: Boolean=false; Logging: Boolean = true);
var
  I: Integer;
  TStr: String;
begin
  if Logging then
    Log('[IRC] > '+S);
  for I := 0 to Length(Users)-1 do
    if Users[I].InChannel[Channel.Number] then
      if not ((ExceptSelf) and (Users[I] = Self)) then
        Users[I].SendLn(S,false);
end;

procedure TUser.Broadcast(S: string; OnlyChannels: Boolean = True; ExceptSelf: Boolean=false; Logging: Boolean = True);
var
  I,J: Integer;
  TStr: String;
begin
  if Logging then
    Log('[IRC] > '+S);
  if OnlyChannels then
  begin
    for I := 0 to Length(Channels)-1 do
      if InChannel[Channels[I].Number] then
        for J := 0 to Length(Users)-1 do
          if Users[J].InChannel[Channels[I].Number] then
            if not ((ExceptSelf) and (Users[J] = Self)) then
              Users[J].SendLn(S,false);
  end
  else
    for I := 0 to Length(Users)-1 do
      if not ((ExceptSelf) and (Users[I] = Self)) then
        Users[I].SendLn(S,false);
end;

procedure TUser.Die(Action, Reason, Master: String);
var
  I: Integer;
begin
  LastSenior:=Master;
  QuitMsg:=UpperFCStr(Action)+' by '+Master+': '+Reason;
  Quit:=true;
  if not Modes['i'] then
    Broadcast(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+QuitMsg);  
  for I:=0 to Length(Channels)-1 do
    if InChannel[Channels[I].Number] then
      InChannel[Channels[I].Number] := False;
  SendLn('ERROR :You have been '+LowerFCStr(Action)+' by '+Master+': '+Reason);
  if Registered then
    AddSeen(Nickname,QuitMsg);
  if (Socket <> 0) then closesocket(Socket); Socket:=0;
end;

function TUser.ContextCommand(S: String): Boolean;
var Command: string;
begin
  Result:=false;
  if Pos('$',S) = 1 then
  begin
    Result:=true;
    Delete(S,1,1);
    Command:=Copy(S, 1, Pos(' ', S+' ')-1);
    if not ((TextMatch(Command,'PRIVMSG')) or (TextMatch(Command,'NOTICE'))) then
    begin
      EventLog('[COMMAND] <'+Nickname+'> $'+S);
      ExecuteCommand(S);
    end
    else
      EventLog('[IGNORED] <'+Nickname+'> $'+S);
  end
end;

function TUser.Registered: Boolean;
begin
  if (Nickname='') or (Username='') then
    Result:=false
  else
    Result:=true;
end;

function TUser.Authorized: Boolean;
begin
  Result:=false;
  if UserPass <> '' then
    if (UserPass=IRCPassword) or (UserPass=IRCPassword2) then
      Result:=true;
end;

function TUser.ChangeMode(Side, Mode: Char; Master: String): Boolean;
var
  I, J: Integer;
  Pre, LAction: String;
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
        LAction:=L_IRC_ACTION_MUTE;
        LastBanTime:=IRCDateTime(Now);
      end
      else
      begin
        Pre:='un';
        LAction:=L_IRC_ACTION_UNMUTE;
      end;
      ServerMessage('You have been '+Pre+'muted by '+Master+'.');
      EventLog(Format(LAction, [Master, Nickname]));
    end

    else
      if Mode='i' then
      begin
        if Side = '+' then
        begin
          Pre:='in';
          LAction:=L_IRC_ACTION_HIDE;
        end
        else
        begin
          Pre:='';
          LAction:=L_IRC_ACTION_UNHIDE;
        end;
        ServerMessage('You have been made '+Pre+'visible by '+Master+'.');
        EventLog(Format(LAction, [Master, Nickname]));
      end
    else
      if (Mode='a')or(Mode='q') then
      begin
        if Side = '+' then
        begin
          if not ((Modes['a']) or (Modes['q'])) then
          begin
            SetLength(Supers,Length(Supers)+1);
            Supers[Length(Supers)-1].Nick:=Nickname;
            Supers[Length(Supers)-1].IP:=ConnectingFrom;
          end;
        end
        else
        begin
          if (not (Modes['q']) and (Modes['a']) and (Mode='a'))
             or (not (Modes['a']) and (Modes['q']) and (Mode='q'))
          then
          begin
            for I:=Length(Supers)-1 downto 0 do
              for J:=I to Length(Supers)-2 do
                Supers[J]:=Supers[J+1];
            SetLength(Supers, Length(Supers)-1);
          end;
        end;
      end;

    if Mode='i' then
      if Side = '+' then
        Broadcast(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+Channels[I].Name, true, true)
      else
        Broadcast(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN '+Channels[I].Name, true, true)
    else
      Broadcast(':'+Master+' MODE '+Channels[I].Name+' '+Side+Mode+' '+Nickname);

    if (Mode<>'b')and(Mode<>'i') then
      EventLog(Format(L_IRC_ACTION_MODE, [Master, Side+Mode, Nickname]));

    Result:=true;
  end
  else
    Result:=false;
end;

function TUser.DataFloodCheck(DataLen: Integer): Boolean;
var
  Timelapse: Int64;
begin
  Result := false;
  if not White then
  begin
    Timelapse:=MillisecondsBetween(Now, LastDataTime);
    if Timelapse > DataStats then
      DataStats:=0
    else
      DataStats:=DataStats-Timelapse;
    LastDataTime:=Now;
    DataStats:=Round((DataStats+1000+DataLen*8)*AntiFloodFactor);
    if DataStats > 30000 then
      Result:= true;
  end;
end;

function TUser.MessageFloodCheck(MsgLen: Integer): Boolean;
var
  Timelapse: Int64;
begin
  Result := false;
  if not White then
  begin
    Timelapse:=MillisecondsBetween(Now, LastMessageTime);
    if Timelapse > FloodPoints then
      FloodPoints:=0
    else
      FloodPoints:=FloodPoints-Timelapse;
    LastMessageTime:=Now;
    FloodPoints:=Round(FloodPoints*1.4)+3000+MsgLen*12;
    if FloodPoints*AntiFloodFactor > 20000 then
      Result:= true;
  end;
end;
procedure TUser.ResumeThread;
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
begin
    Name:=Name;
    Scheme:=Scheme;
    Topic:=Topic;
    CreationTime:=IRCDateTime(Now);
    Number:=Length(Channels)-1;
end;

destructor TChannel.Destroy;
var I: Integer;
// ***************************************************************

function GetFormattedUserCount: TUserCount;
var
  I: Integer;
  User: TUser;
  UserList: TList;
begin
  Result.Invisible := 0;
  Result.Operators := 0;
  Result.Unknown := 0;
  Result.Registered := 0;
  UserList := UserThreadList.LockList;
  Result.All := UserList.Count;
  for I:=0 to UserList.Count-1 do
  begin
    User:=UserList[I];
    if User.Registered then
    begin
      Inc(Result.Registered);
      if (User.Modes['q'])or(User.Modes['a'])or(User.Modes['o']) then
        Inc(Result.Operators);
      if User.Modes['i'] then
        Inc(Result.Invisible);
    end
    else
      Inc(Result.Unknown);
  end;
  UserThreadList.UnlockList;
end;

function GetRegisteredUserCount: u_int;
var UserCount: TUserCount;
begin
  UserCount:=GetFormattedUserCount;
  Result:=UserCount.Registered;
end;

function GetUserCount: u_int;
var
  UserList: TList;
begin
  UserList:=UserThreadList.LockList;
  Result:=UserList.Count;
  UserThreadList.UnlockList;
end;

function GetChannelCount: u_int;
var
  ChannelList: TList;
begin
  ChannelList:=ChannelThreadList.LockList;
  Result:=ChannelList.Count;
  ChannelThreadList.UnlockList;
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
    EventLog('[IRC] '+L_IRC_CHANNEL_NOT_FOUND);
    AddChannel('#AnythingGoes','Pf,Be','00 Open games with ''rope knocking'' allowed & blood fx');
    EventLog('[IRC] '+Format(L_IRC_CHANNEL_ADDED, ['#AnythingGoes']));
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

      if Length(Name) > 30 then
        Name:=Copy(Name, 1, 30);

      if (Name='') or (Name='#') then Name:='#AnythingGoes';
      if Name[1] <> '#' then Name:='#'+Name;

      if not AddChannel(Name,Scheme,Topic) then
        EventLog('[IRC] '+Format(L_IRC_CHANNEL_ALREADY, [Name]))
      else
        EventLog('[IRC] '+Format(L_IRC_CHANNEL_ADDED, [Name]));
      Inc(N);
    end;
    if N=1 then
    begin 
      EventLog('[IRC] '+L_IRC_CHANNEL_EMPTY);
      AddChannel('#AnythingGoes','Pf,Be','00 Open games with ''rope knocking'' allowed & blood fx');
      EventLog('[IRC] '+Format(L_IRC_CHANNEL_ADDED, ['#AnythingGoes']));
    end;
  end;
end;

procedure AddSeen(Nick, QuitMsg: String);
var I: Integer;
begin
  if Length(Seens) < 1 then SetLength(Seens,1);
  for I:=0 to Length(Seens)-1 do
    if TextMatch(Nick,Seens[I].Nick) then
    begin
      Seens[I].LastSeen:=Now;
      Seens[I].QuitMsg:=QuitMsg;
      Break;
    end
    else if I=Length(Seens)-1 then
// ***************************************************************

function LockUserByIP(IP: String): TUser;
var
  I: Integer;
  UserList: TList;
  User: TUser;
begin
  Result := nil;
  UserList:=UserThreadList.LockList;
  if IP <> '' then
    for I:=0 to UserList.Count-1 do
    begin
      User:=UserList[I];
      if TextMatch(User.ConnectingFrom,IP) then
      begin
        Result := User;
        Break;
      end;
    end;
end;

function LockUserByName(Name: String): TUser;
var
  I: Integer;
  UserList: TList;
  User: TUser;
begin
  Result := nil;
  UserList:=UserThreadList.LockList;
  if Name <> '' then
    for I:=0 to UserList.Count-1 do
    begin
      User:=UserList[I];
      if TextMatch(User.Nickname,Name) and User.Registered then
      begin
        Result := User;
        Break;
      end;
    end;
end;

function LockChannelByName(Name: String): TChannel;
var
  I: Integer;
  ChannelList: TList;
  Channel: TChannel;
begin
  Result := nil;
  ChannelList:=ChannelThreadList.LockList;
  if Name <> '' then
    for I:=0 to ChannelList.Count-1 do
    begin
      Channel:=ChannelList[I];
      if TextMatch(Channel.Name,Name) then
      begin
        Result := Channel;
        Break;
      end;
    end;
end;

function LockSuperByName(Name: String): TSuper;
var
  I: Integer;
  SuperList: TList;
  Super: TSuper;
begin
  Result := nil;
  SuperList:=SuperThreadList.LockList;
  if Name <> '' then
    for I:=0 to SuperList.Count-1 do
    begin
      Super:=SuperList[I];
      if TextMatch(Super.Name,Name) then
      begin
        Result := Super;
        Break;
      end;
    end;
end;

function LockSuperByIP(IP: String): TSuper;
var
  I: Integer;
  SuperList: TList;
  Super: TSuper;
begin
  Result := nil;
  SuperList:=SuperThreadList.LockList;
  if IP <> '' then
    for I:=0 to SuperList.Count-1 do
    begin
      Super:=SuperList[I];
      if TextMatch(Super.IP,IP) then
      begin
        Result := Super;
        Break;
      end;
    end;
end;

function SuperNameExists(Name: String): Boolean;
var Super: TSuper;
begin
  Result := False;
  Super := LockSuperByName(Name);
  if (Super <> nil) then
    Result := True;
  SuperThreadList.UnlockList;
end;

function SuperIPExists(IP: String): Boolean;
var Super: TSuper;
begin
  Result := False;
  Super := LockSuperByIP(IP);
  if (Super <> nil) then
    Result := True;
  SuperThreadList.UnlockList;
end;

function NickInUse(Nick: String): Boolean;
var User: TUser;
begin
  Result := False;
  User := LockUserByName(Nick);
  if (User <> nil) then
    Result := True;
  UserThreadList.UnlockList;
end;

function ChannelExists(Name: String): Boolean;
var Channel: TChannel;
begin
  Result := False;
  Channel := LockChannelByName(Name);
  if (Channel <> nil) then
    Result := True;
  ChannelThreadList.UnlockList;
end;

function ForbiddenNick(Nick: String): Boolean;
begin
  Result := False;
  if ULPos('serve',Nick) <> 0 then
    Result := True
  else
    if ULPos('admin',Nick) <> 0 then
      Result := True
  else
    if ULPos('HostingBud',Nick) <> 0 then
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
    EventLog('[IRC] '+L_BIND_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    EventLog('[IRC] '+L_BIND_ERROR+' ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  EventLog('[IRC] '+L_LISTENING+' '+IntToStr(IRCPort)+'.');

  GetChannels;

  repeat
    T:=SizeOf(incoming);
    AcceptSocket := accept( m_socket, @incoming, @T );
    if (AcceptSocket<>INVALID_SOCKET) then
    begin
      if not BannedIP(String(inet_ntoa(incoming.sin_addr))) then
      begin
        T:=SizeOf(incoming);
        Log('[IRC] '+L_CONNECTION_ESTABLISHED+' '+inet_ntoa(incoming.sin_addr));
        Inc(IRCConnections);

        User:=TUser.Create(true);
        SetLength(User.InChannel,Length(Channels));
        User.SignonTime:=IRCDateTime(Now);
        User.Socket:=AcceptSocket;
        User.ConnectingFrom:=String(inet_ntoa(incoming.sin_addr));
        User.LastSenior:='server';
        User.QuitMsg:='Disconnected';
        User.FloodPoints:=0;
        User.Quit:=false;
        User.Away:=false;
        User.LastMessageTime:=Now;
    //  User.Modes['s']:=True;
        SetLength(Users, Length(Users)+1);
        Users[Length(Users)-1]:=User;
      end
      else
      begin
        EventLog(Format(L_REQUEST_REJECTED, [inet_ntoa(incoming.sin_addr), IntToStr(IRCPort)]));
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
      SendLn(':'+ServerHost+' NOTICE '+Nickname+' :'+S, false);
end;

var
  ThreadID: Cardinal = 0;

procedure StartIRCServer;
begin
  if ThreadID=0 then  // start only once
    CreateThread(nil, 0, @MainProc, nil, 0, ThreadID);
end;

initialization
  UserThreadList:=TThreadList.Create;
  ChannelThreadList:=TThreadList.Create;
  SeenThreadList:=TThreadList.Create;
  SuperThreadList:=TThreadList.Create;

finalization
  UserThreadList.Free;
  ChannelThreadList.Free;
  SeenThreadList.Free;
  SuperThreadList.Free;

end.
