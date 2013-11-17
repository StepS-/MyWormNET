unit IRCServer;
// a quick hack of an IRC server, which supports only one channel

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
    InChannel: Boolean;
    Modes: array[char] of Boolean;
    procedure Execute; override;
    procedure SendLn(S: string);
    end;

var
  Users: array of TUser;

procedure StartIRCServer;
procedure LogToOper(S: string);

resourcestring
  IRCPassword='ELSILRACLIHP ';
  IRCPassword2='ELSILRACLIHP';
  ValidNickChars='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`_-|';

implementation
uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Base, Data, SysUtils, HTTPServer, WormNATServer;

procedure TUser.Execute;
var
  Buffer, S, S2, S3, S4, Command, Target, Description: string;
  R, Bytes, I, J, K, N, M: Integer;
  PingTimer: Integer;
  B: Boolean;
  ReadSet, ErrorSet: record
    count: u_int;
    Socket: TSocket;
    end;
  TimeVal: TTimeVal;
  User: TUser;
  Password: string;
  C: Char;
  
  procedure LogIn;
  var I: Integer;
  begin
    EventLog(Nickname+' ('+ConnectingFrom+') logged in.');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :Welcome, '+Nickname+' !');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :This is a minimal WormNet-compatible IRC server emulator,');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :supporting only the base set of IRC features.');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :The server software was written by ');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :The_CyberShadow <thecybershadow@gmail.com>');
    SendLn(':'+ServerHost+' 001 '+Nickname+' :and extended by StepS.');
    SendLn(':'+ServerHost+' 003 '+Nickname+' :This server was created '+StartupTime);
    SendLn(':'+ServerHost+' 005 '+Nickname+' WALLCHOPS PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ CHANTYPES=# MAXCHANNELS=20 MAXBANS=25 NICKLEN=15 TOPICLEN=120 KICKLEN=90 NETWORK='+NetworkName+' CHANMODES=b,k,l,imnpstr MODES=6 :are supported by this server');
    if WormNATPort>0 then
      SendLn(':'+ServerHost+' 001 '+Nickname+' :[WormNATRouteOn:'+IntToStr(WormNATPort)+'] This server supports built-in WormNAT routing.');
    //SendLn(':'+ServerHost+' 007 '+Nickname+' :[YourIP:'+ConnectingFrom+'] Your external IP address is '+ConnectingFrom+'.');
    //SendLn(':'+ServerHost+' 004 '+Nickname+' wormnet1.team17.com 2.8/hybrid-6.3.1 oOiwszcrkfydnxb biklmnopstve');
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
    SendLn(':'+ServerHost+' 254 '+Nickname+' 1 :channel hard-coded limit');
    SendLn(':'+ServerHost+' 375 '+Nickname+' :- '+ServerHost+' Message of the Day - ');
    S:=GetFile('motd.txt')+#13#10;
    while GetLine(S, S2) do
     if(S<>'')or(S2<>'') then
      SendLn(':'+ServerHost+' 372 '+Nickname+' :- '+S2);
    SendLn(':'+ServerHost+' 376 '+Nickname+' :End of /MOTD command.');
  end;
  
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
        WriteLn('< '+S);
        Command:=UpperCase(Copy(S, 1, Pos(' ', S+' ')-1));
        Delete(S, 1, Length(Command)+1);

        if Command='IPLOOKUP' then
          begin
          if S <> '' then
            begin
              if (Modes['q'])or(Modes['a'])or(Modes['o']) then
                begin
                for I:=0 to Length(Users)-1 do
                  if S=Users[I].Nickname then
                    begin
                      SendLn(':SERVER'#160'MESSAGE!SERVER@'+ServerHost+' PRIVMSG '+Nickname+' :IP of user '+S+' is: '+Users[I].ConnectingFrom+'.');
                      Break
                    end
                  else if I=Length(Users)-1 then
                    SendLn(':'+ServerHost+' 401 '+Nickname+' '+S+' :Failed to find an user with this nickname');
                end
              else
                SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to execute the command');
            end
            else
              SendLn(':'+ServerHost+' 461 '+Nickname+' '+Command+' :Insufficient parameters')
          end
        else
        if (Command='MUTE')or(Command='UNMUTE') then
          begin
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
                        B:=false;

                          if (Command='MUTE') then
                            begin
                            if not Users[I].Modes['b'] then
                              begin
                              Users[I].Modes['b'] := true;
                              Users[I].LastBanTime:=IRCDateTimeNow;
                              B:=true;
                              end;
                            C:='+';
                            end

                          else
                            begin
                            if Users[I].Modes['b'] then
                              begin
                              Users[I].Modes['b'] := false;
                              B:=true;
                              end;
                            C:='-';
                            end;

                        if B then
                          begin
                          Users[I].LastSenior:=Nickname;

                          if Users[I].InChannel then
                            for J:=0 to Length(Users)-1 do
                              if Users[J].InChannel then
                                Users[J].SendLn(':'+Nickname+' MODE '+IRCChannel+' '+C+'b '+Users[I].Nickname);
                          Users[I].SendLn(':SERVER'#160'MESSAGE!SERVER@'+ServerHost+' PRIVMSG '+Users[I].Nickname+' :You have been '+LowerCase(Command)+'d by '+Nickname+'.');
                          Break
                          end
                        else
                          begin
                          if Command='MUTE' then S3:='already'
                          else S3:='not';
                          SendLn(':'+ServerHost+' 401 '+Nickname+' '+S+' :This user has '+S3+' been muted');
                          end;
                      end
                    else
                      begin
                        SendLn(':'+ServerHost+' 484 '+Nickname+' '+S+' :Cannot '+LowerCase(Command)+' this user due to unsufficient privileges');
                        Break
                      end;
                    end
                  else if I=Length(Users)-1 then
                    SendLn(':'+ServerHost+' 401 '+Nickname+' '+S+' :Failed to find an user with this nickname');
                end
              else
                SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to execute the command');
            end
            else
              SendLn(':'+ServerHost+' 461 '+Nickname+' '+Command+' :Insufficient parameters');
          end
        else
        if (Command='KICK') or (Command='KILL') then
         begin
          if S <> '' then
           begin
            if Pos(IRCChannel, S) <> 0 then Delete(S, 1, Pos(' ', S));
            if Pos(' ', S) <> 0 then
              begin
                Target:=Copy(S, 1, Pos(' ', S)-1);
                Description:=Copy(S, Pos(' ', S)+1, Length(S));
                if Copy(Description, 1, 1) = ':' then Delete(Description, 1, 1);
              end
            else
              begin
                Target:=Copy(S, 1, Length(S));
                Description:='No reason specified';
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
                        if Users[I].InChannel then Users[I].InChannel := False;
                        if not Users[I].Modes['i'] then
                          for J:=0 to Length(Users)-1 do
                            Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' QUIT :Kicked by '+Nickname+': '+Description);
                        Users[I].SendLn('ERROR :You have been kicked from the server by '+Nickname+': '+Description);
                        if (Users[I].Socket <> 0) then closesocket(Users[I].Socket); Users[I].Socket:=0;
                        Break
                      end
                    else
                      begin
                        SendLn(':'+ServerHost+' 484 '+Nickname+' '+Target+' :Cannot kick this user due to unsufficient privileges');
                        Break
                      end;
                  end
                else if I=Length(Users)-1 then
                  SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :Failed to find an user with this nickname');
             end
            else
              begin
              if (Modes['h']) then
                Description:='. You can use /MUTE to mute this user, though.'
              else
                Description:='';
              SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to execute the command'+Description);
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
              SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :Failed to find an user with this nickname');
          end
        else
        if Command='PRANK' then
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

            if (Modes['q'])or(Modes['a'])or(Modes['o']){or(Modes['h'])} then
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
                          Users[I].SendLn('ERROR :'+Description);
                          Break
                        end
                      else
                        begin
                          SendLn(':'+ServerHost+' 484 '+Nickname+' '+Target+' :Cannot prank this user due to unsufficient privileges');
                          Break
                        end;
                      end
                    else if I=Length(Users)-1 then
                      SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :Failed to find an user with this nickname');
          end
        else
        if Command='KICKALL' then
          begin
            if Modes['q'] then
              begin
              for I:=0 to Length(Users)-1 do
                if Users[I].Modes['q'] = false then
                  begin
                    if not Users[I].Modes['i'] then
                      for J:=0 to Length(Users)-1 do
                        Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' QUIT :Massive kicking started by '+Nickname);
                    Users[I].LastSenior:=Nickname;
                    Users[I].SendLn('ERROR :Massive kicking started by '+Nickname);
                    if (Users[I].Socket <> 0) then closesocket(Users[I].Socket); Users[I].Socket:=0;
                  end;
              end
            else
              SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :You must be an owner');
          end
        else
        if Command='SENDRAW' then
          begin
           Target:='';
           Description:='';
           if S <> '' then
             begin
             if Pos(' ', S) <> 0 then
                begin
                  Target:=Copy(S, 1, Pos(' ', S)-1);
                  Description:=Copy(S, Pos(' ', S)+1, Length(S));
                end;
             end;

             if (Modes['q']){or(Modes['a'])or(Modes['o'])} then
              begin
              if (Target='') or (Description='') then
                SendLn(':'+ServerHost+' 461 '+Nickname+' '+Command+' :Insufficient parameters')
              else
                begin
                for I:=0 to Length(Users)-1 do
                  begin
                  if Target=Users[I].Nickname then
                    begin
                      if (Modes['q'])
               //     or ((Modes['a']) and not (Users[I].Modes['q']))
               //     or ((Modes['o']) and not (Users[I].Modes['a']) and not (Users[I].Modes['q']))
                      or (Users[I] = Self) then
                        begin
                        for J:=0 to Length(Users)-1 do
                          if Users[J] <> Users[I] then
                            Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' '+Description);
                        Break
                        end
                      else
                        begin
               //         SendLn(':'+ServerHost+' 484 '+Nickname+' '+Target+' :Cannot sendraw this user due to insufficient privileges');
                          Break
                        end;
                    end
                  else if I=Length(Users)-1 then
                      SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :Failed to find an user with this nickname');
                  end;
                 end;
                end
               else
                SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to execute this command');
           end
        else
        if Command='ANNOUNCE' then
          begin
            if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
              begin
              if S <> '' then
                for I:=0 to Length(Users)-1 do
                  Users[I].SendLn(':SERVER'#160'ANNOUNCEMENT!SERVER@'+ServerHost+' NOTICE '+IRCChannel+' :'+S)
              else
                SendLn(':'+ServerHost+' 412 '+Nickname+' '+Command+' :No text to send');
              end
            else
              SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to execute the command');
          end
        else
     //   if Command='SERVERRAW' then

        if Command='ISON' then
         begin
          if Nickname <> '' then
          begin
          Description:='';
          while Pos(' ',S) <> 0 do
            begin
            Target:=Copy(S, 1, Pos(' ', S)-1);
            Delete(S, 1, Pos(' ', S));
            for I:=0 to Length(Users)-1 do
              if Target=Users[I].Nickname then
                Description:=Description+Target+' ';
            end;
          if S='' then
            SendLn(':'+ServerHost+' 303 '+Nickname+' :'+Description)
          else
            begin
            Target:=S;
            for I:=0 to Length(Users)-1 do
              if Target=Users[I].Nickname then
                Description:=Description+Target;
            SendLn(':'+ServerHost+' 303 '+Nickname+' :'+Description);
            end;
          end;
         end
        else
        if Command='PING' then
          SendLn('PONG :'+ServerHost)
        else
        if Command='PONG' then
        else
        if Command='AWAY' then
        else
        if Command='WHOIS' then
        else
        if Command='PASS' then {ignore}
          begin
          Password:=S;
          {if (Password<>IRCPassword) and (Password<>IRCPassword2) then
            begin
            SendLn(':'+ServerHost+' 464 '+S+' :Password incorrect');
            raise Exception.Create('Bad password!');
            end;}
          end
        else
        if Command='NICK' then
        begin
          {if (Password<>IRCPassword) and (Password<>IRCPassword2) then
            begin
            SendLn(':'+ServerHost+' 464 '+S+' :Password incorrect');
            raise Exception.Create('Bad password!');
            end;}

          if Nickname<>'' then
            SendLn(':'+ServerHost+' 400 '+Nickname+' '+S+' :Nick change isn''t supported.')
          else
          begin
            for I:=Length(S) downto 1 do
              if Pos(S[I], ValidNickChars)=0 then
                Delete(S, I, 1);
            if S='' then
              SendLn(':'+ServerHost+' 432 '+Nickname+' '+S+' :Erroneous nickname')
            else
            begin
              B := False;
              for I:=0 to Length(Users)-1 do
                if UpperCase(Users[I].Nickname)=UpperCase(S) then
                  B := True;
              if B then
                SendLn(':'+ServerHost+' 433 '+Nickname+' '+S+' :Nickname is already in use')
              else
                Nickname:=S;
              if Nickname<>'' then
                LogIn;
            end;
          end;
        end
        else
        // USER Username hostname servername :40 0 RO
        if Command='USER' then
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
                LogIn;
            end
            else
              SendLn(':'+ServerHost+' 462 '+Nickname+' :You may not reregister');
          end
        else
        if Command='QUIT' then
          begin
          // :CyberShadow!cybershado@38F7DF98.502358C0.F6DD7E74.IP QUIT :Input/output error
          if (InChannel) and not (Modes['i']) then
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+Copy(S, 2, 1000));
          InChannel := False;
          Break
          end
        else
        if Command='JOIN' then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
          if InChannel then
            SendLn(':'+ServerHost+' 403 '+Nickname+' '+S+' :You already are in a channel')
          else
          if S=IRCChannel then
            begin
            EventLog(Nickname+' ('+ConnectingFrom+') has joined '+IRCChannel);
            InChannel:=True;
            //:CyberShadow-MD!Username@no.address.for.you JOIN :#AnythingGoes
            if not Modes['i'] then
              for I:=0 to Length(Users)-1 do
                begin
                if Users[I].InChannel then
                  begin
                  Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN :'+IRCChannel);
                  S3:='qaohv';
                  for K:=1 to 5 do
                    if Modes[S3[K]] then
                      Users[I].SendLn(':'+ServerHost+' MODE '+IRCChannel+' +'+S3[K]+' '+Nickname);
                  end;
                end
            else
              SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' JOIN :'+IRCChannel);
            S:=':'+ServerHost+' 353 '+Nickname+' = '+IRCChannel+' :';
            for I:=0 to Length(Users)-1 do
              if (Users[I].InChannel) and not (Users[I].Modes['i']) then
                begin
                if Users[I].Modes['q'] then
                  S:=S+'~';
                if Users[I].Modes['a'] then
                  S:=S+'&';
                if Users[I].Modes['o'] then
                  S:=S+'@';
                if Users[I].Modes['h'] then
                  S:=S+'%';
                if Users[I].Modes['v'] then
                  S:=S+'+';
                S:=S+Users[I].Nickname+' ';
                end;
            SendLn(S);
            SendLn(':'+ServerHost+' 366 '+Nickname+' '+IRCChannel+' :End of /NAMES list.');
            end
          else
            SendLn(':'+ServerHost+' 403 '+Nickname+' '+S+' :No such channel');
          end
        else
        if Command='NAMES' then
          begin
          S:=':'+ServerHost+' 353 '+Nickname+' = '+IRCChannel+' :';
            for I:=0 to Length(Users)-1 do
              if (Users[I].InChannel) and not (Users[I].Modes['i']) then
                begin
                if Users[I].Modes['q'] then
                  S:=S+'~';
                if Users[I].Modes['a'] then
                  S:=S+'&';
                if Users[I].Modes['o'] then
                  S:=S+'@';
                if Users[I].Modes['h'] then
                  S:=S+'%';
                if Users[I].Modes['v'] then
                  S:=S+'+';
                S:=S+Users[I].Nickname+' ';
                end;
            SendLn(S);
          end
        else
        if Command='PART' then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
            begin
            if InChannel then
              begin
              EventLog(Nickname+' ('+ConnectingFrom+') has left '+IRCChannel);
              if Modes['i'] then
                SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+S)
              else
                for I:=0 to Length(Users)-1 do
                  if Users[I].InChannel then
                    Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' PART '+S);
              InChannel:=False;
              end;
            end;
          end
        else
        if Command='MODE' then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
            begin
            Target:=Copy(S, 1, Pos(' ', S+' ')-1);
            Description:='';
        //    Delete(S, 1, Pos(':', S+':')-1);
        //    if S<>'' then
        //      SendLn(':'+ServerHost+' 472 '+Nickname+' :Sorry, you can''t set modes for anything.')
        //    else
              if Target=IRCChannel then
                begin
                Delete(S,1,Pos(IRCChannel,S));
                if Pos(' ', S) <> 0 then
                  begin
                  Delete(S,1,Pos(' ',S));
                  if (Pos('+',S))or(Pos('-',S)) = 1 then
                    begin
                    if Pos(' ', S) <> 0 then
                      begin
                      if (Modes['q'])or(Modes['a'])or(Modes['o'])or(Modes['h']) then
                        begin
                        Description:=Copy(S, 1, Pos(' ', S));
                        Delete(S, 1, Pos(' ', S));
                        Target:=S;
                        for I:=0 to Length(Users)-1 do
                          begin
                          if Users[I].Nickname=Target then
                            begin
                            for K:=2 to Pos(' ',Description)-1 do
                              begin
                              B:=False;
                              S:=Description[K];
                              C:=Description[1];
                              if (S<>' ') then
                                begin
                                if not
                                  (
                                    ((S='a') or (S='q') and not (Modes['q']))
                                    or (not (S='v') and not (S='b') and ((Modes['h'])
                                        and not (Modes['o']) and not (Modes['a']) and not (Modes['q'])))
                                    or ((S='L') and (Target<>Nickname))
                                    or ((Users[I].Modes['q']) and not (Modes['q']))
                                    or ((Users[I].Modes['a']) and not (Modes['q']))
                                    or ((Users[I].Modes['o']) and not (Modes['o']) and not (Modes['a']) and not (Modes['q']))
                                  )
                                then
                                  begin
                                  if C = '+' then
                                    begin
                                    if Users[I].Modes[S[1]]=false then
                                      begin
                                      Users[I].Modes[S[1]]:=True;
                                      S3:='';
                                      B:=True;
                                      end;
                                    end
                                  else if C = '-' then
                                    begin
                                    if Users[I].Modes[S[1]]=true then
                                      begin
                                      Users[I].Modes[S[1]]:=False;
                                      S3:='un';
                                      B:=True;
                                      end
                                    end;
                                  end
                                  else
                                    if ((S='L') and (Target<>Nickname)) then
                                      SendLn(':'+ServerHost+' 502 '+Nickname+' '+Command+' :You can only enable logging for yourself')
                                    else
                                      begin
                                      SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to change mode '+S+' for a given user');
                                      Break;
                                      end;
                                end;
                              if B then
                                begin
                                SendLn(':'+Nickname+' MODE '+Users[I].Nickname+' :'+C+S);
                                if S='b' then
                                  begin
                                  Users[I].LastSenior:=Nickname;
                                  if C = '+' then
                                    Users[I].LastBanTime:=IRCDateTimeNow;
                                  Users[I].SendLn(':SERVER'#160'MESSAGE!SERVER@'+ServerHost+' PRIVMSG '+Users[I].Nickname+' :You have been '+S3+'muted by '+Nickname+'.');
                                  end;
                                if Users[I].InChannel then
                                  for J:=0 to Length(Users)-1 do
                                    if Users[J].InChannel then
                                      if S='i' then
                                        begin
                                        if Users[I].Nickname <> Users[J].Nickname then
                                          Users[J].SendLn(':'+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' PART '+IRCChannel);
                                        end
                                      else
                                        Users[J].SendLn(':'+Nickname+' MODE '+IRCChannel+' '+C+S+' '+Users[I].Nickname);
                                end;
                              end;
                            Break
                            end
                            else if I = Length(Users)-1 then
                              SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :Failed to find an user with this nickname');
                          end;
                        end
                        else
                          SendLn(':'+ServerHost+' 481 '+Nickname+' '+Command+' :Insufficient privileges to change user modes');
                      end
                      else
                        if Pos('+b',S) <> 0 then
                        begin
                          for I:=0 to Length(Users)-1 do
                            if Users[I].Modes['b'] then
                              SendLn(':'+ServerHost+' 367 '+Nickname+' '+IRCChannel+' '+Users[I].Nickname+'!'+Users[I].Username+'@'+StealthIP+' '+Users[I].LastSenior+' '+IntToStr(Users[I].LastBanTime));
                        SendLn(':'+ServerHost+' 368 '+Nickname+' '+IRCChannel+' :End of Channel Ban List');
                        end;
                    end
                    else
                      SendLn(':'+ServerHost+' 324 '+Nickname+' '+IRCChannel+' +tn');
                  end
                  else
                    SendLn(':'+ServerHost+' 324 '+Nickname+' '+IRCChannel+' +tn');
                end
                else
                  if S='' then
                    SendLn(':'+ServerHost+' 324 '+Nickname+' '+IRCChannel+' +tn')
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
          end
        else
        if(Command='PRIVMSG')or(Command='NOTICE') then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
          if Modes['b'] then
            SendLn(':SERVER'#160'MESSAGE!SERVER@'+ServerHost+' PRIVMSG '+Nickname+' :Sorry, but you are muted by '+LastSenior+' and thus cannot talk.')
          else
            begin
            Target:=Copy(S, 1, Pos(' ', S+' ')-1);
            Delete(S, 1, Pos(':', S+':')-1);
            if Target=IRCChannel then
              begin
              EventLog('['+IRCChannel+'] <'+Nickname+'> '+Copy(S, 1, 1000));
              for I:=0 to Length(Users)-1 do
                if Users[I].InChannel and (Users[I]<>Self)then
                  Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' '+Command+' '+IRCChannel+' '+S);
              end
            else
              begin
              User:=nil;
              S3:='~&@%+';
              for K:=1 to 5 do
                if Pos(S3[K],Target) <> 0 then
                  Delete(Target,Pos(S3[K],Target),1);
              for I:=0 to Length(Users)-1 do
                if LowerCase(Users[I].Nickname)=LowerCase(Target) then
                  User:=Users[I];
              if User=nil then
                SendLn(':'+ServerHost+' 401 '+Nickname+' '+Target+' :No such nick/channel.')
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
          end
        else
        if (Command='OPER')or(Command='TAKEOWN') then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
            begin
            if Copy(S, 1, Pos(' ', S+' ')-1)<>IRCOperPassword then
              Delete(S, 1, Pos(' ', S+' '));  // ignore username
            if S=IRCOperPassword then
              begin
              if Command='OPER' then
                begin
                Description:='Operator';
                C:='o';
                end
              else
                begin
                Description:='Owner';
                C:='q';
                end;
              EventLog(Nickname+' ('+ConnectingFrom+') has registered as an '+Description+'.');
              Modes[C]:=True;
              SendLn(':'+Nickname+' MODE '+Nickname+' :+'+C);
              if InChannel then
                for I:=0 to Length(Users)-1 do
                  if Users[I].InChannel then
                  Users[I].SendLn(':'+ServerHost+' MODE '+IRCChannel+' +'+C+' '+Nickname);
              end
              else
                SendLn(':'+ServerHost+' 464 '+Nickname+' '+Command+' :Incorrect password');
            end;
          end
        else
        if Command='WHO' then
          begin
          //:wormnet1.team17.com 352 Alexis #AnythingGoes Username no.address.for.you wormnet1.team17.com TiCPU H :0 TiCpu
          //:wormnet1.team17.com 315 Alexis * :End of /WHO list.
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
            begin
              for I:=0 to Length(Users)-1 do
                begin
                S4:='';
                if Users[I].Modes['q'] then
                  S4:='~'
                else if Users[I].Modes['a'] then
                  S4:='&'
                else if Users[I].Modes['o'] then
                  S4:='@'
                else if Users[I].Modes['h'] then
                  S4:='%'
                else if Users[I].Modes['v'] then
                  S4:='+';

                if S<>'' then
                  Target:=S
                else
                  Target:=Users[I].Nickname;

                if S<>IRCChannel then
                  begin
                  S3:='~&@%+';
                  for K:=1 to 5 do
                    if Pos(S3[K],S) <> 0 then
                      Delete(S,Pos(S3[K],S),1);
                  if (S<>'') and (S<>Users[I].Nickname) then continue;
                  if Users[I].Nickname <> Nickname then
                    begin
                    if not (Users[I].Modes['i']) then
                      if Users[I].InChannel then
                        SendLn(':'+ServerHost+' 352 '+Nickname+' '+IRCChannel+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Target+' H'+S4+' :0 '+Users[I].Realname)
                      else if Users[I].Nickname <> '' then
                        SendLn(':'+ServerHost+' 352 '+Nickname+' * '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Target+' H'+S4+' :0 '+Users[I].Realname);
                    end
                  else
                    if InChannel then
                      SendLn(':'+ServerHost+' 352 '+Nickname+' '+IRCChannel+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Target+' H'+S4+' :0 '+Realname)
                    else
                      SendLn(':'+ServerHost+' 352 '+Nickname+' * '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Target+' H'+S4+' :0 '+Realname);
                  if (S<>'') and (S=Users[I].Nickname) then Break;
                  end
                else if Users[I].InChannel then
                  begin
                  if (Pos(' o ',S+' ') <> 0) and not ((Users[I].Modes['h']) or (Users[I].Modes['o']) or (Users[I].Modes['a']) or (Users[I].Modes['q']))
                    then continue
                  else
                  if not (Users[I].Modes['i']) then
                    if Users[I].Nickname <> Nickname then
                      SendLn(':'+ServerHost+' 352 '+Nickname+' '+IRCChannel+' '+Users[I].Username+' '+StealthIP+' '+ServerHost+' '+Users[I].Nickname+' H'+S4+' :0 '+Users[I].Realname)
                    else
                      SendLn(':'+ServerHost+' 352 '+Nickname+' '+IRCChannel+' '+Username+' '+ConnectingFrom+' '+ServerHost+' '+Nickname+' H'+S4+' :0 '+Realname);
                  end;
                end;
              if S='' then
                SendLn(':'+ServerHost+' 315 '+Nickname+' * :End of /WHO list.')
              else
                SendLn(':'+ServerHost+' 315 '+Nickname+' '+S+' :End of /WHO list.');
            end;
          end
        else
        if Command='LIST' then
          begin
          if Nickname='' then
            SendLn(':'+ServerHost+' 451 Username '+Command+' :Register first.')
          else
            begin
            N:=0;
            for I:=0 to Length(Users)-1 do
              if Users[I].InChannel then
                Inc(N);
            SendLn(':'+ServerHost+' 321 '+Nickname+' Channel :Users  Name');
            SendLn(':'+ServerHost+' 322 '+Nickname+' '+IRCChannel+' '+IntToStr(N)+' :');
            SendLn(':'+ServerHost+' 323 '+Nickname+' :End of /LIST');
            end
          end
        else
        if Command='EXPECT' then
          begin
          Log('Received EXPECT command from '+ConnectingFrom+' for '+S);
          User:=nil;
          for I:=0 to Length(Users)-1 do
            if Users[I].Nickname=S then
              User:=Users[I];
          if User=nil then
            SendLn(':'+ServerHost+' 401 '+S+' :No such nick.')
          else
            begin
            SendLn(':'+ServerHost+' NOTICE '+Nickname+' :OK, expecting '+User.Nickname+' from '+StealthIP);
            PrepareLink(Self, User);
            end;
          end
        else
        if Command='TIME' then
          SendLn(':'+ServerHost+' '+IntToStr(IRCDateTimeNow)+' 0 :'+TextDateTimeNow)
        else
        if Command='GAMES' then
          begin
          for I:=0 to Length(Games)-1 do
           with Games[I] do
            begin
            if PassNeeded='0' then Description:='[OPEN]'
            else Description:='[PASS]';
            SendLn(':'+ServerHost+' NOTICE '+Nickname+' :'+Description+' '+Name+' '+HosterNickname+' '+HosterAddress+' | wa://'+HosterAddress+'?gameid='+IntToStr(GameID)+'&Scheme=Pf,Be');
            end;
          SendLn(':'+ServerHost+' NOTICE '+Nickname+' :--- '+IntToStr(Length(Games))+' games total ---');
          end
        else
          SendLn(':'+ServerHost+' 421 '+Nickname+' '+Command+' :Unknown command');
        end;

      Inc(PingTimer);
      if PingTimer=18000 then
        SendLn('PING :'+ServerHost);
      if PingTimer=24000 then
        begin
        if InChannel and not Modes['i'] then
          for I:=0 to Length(Users)-1 do
            if Users[I].InChannel then
              Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :Ping timeout');
        if (Socket <> 0) then closesocket(Socket); Socket:=0;
        Break;
        end;
    until Socket=0;
    Log('[IRC] Closing link to '+ConnectingFrom);
    closesocket(Socket);

  except
    on E: Exception do
      begin
      if InChannel and not Modes['i'] then
        for I:=0 to Length(Users)-1 do
          if Users[I].InChannel then
            try
              Users[I].SendLn(':'+Nickname+'!'+Username+'@'+StealthIP+' QUIT :'+E.Message);
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

procedure TUser.SendLn(S: string);
begin
  if Socket=0 then Exit;
  WriteLn('['+TimeToStr(Now)+'] > '+S);
  S:=S+#13#10;
  if send(Socket, S[1], Length(S), 0)<>Length(S) then
    begin
    Socket:=0;  // avoid infinite recursion
    Log('[IRC > Failed ('+WinSockErrorCodeStr(WSAGetLastError)+') ]');
    end;
end;

// ***************************************************************

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
    Log('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  if listen( m_socket, 1 )=SOCKET_ERROR then
    begin
    Log('[IRC] bind error ('+WinSockErrorCodeStr(WSAGetLastError)+').');
    Exit;
    end;
  Log('[IRC] Listening on port '+IntToStr(IRCPort)+'.');

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
