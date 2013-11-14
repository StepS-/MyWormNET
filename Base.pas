unit Base;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

var
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort: Integer;
  IRCOperPassword: string;
  IRCChannel: string;
  StealthIP: string;
  NetworkName: string;

procedure Log(S: string; DiskOnly: Boolean=False);
procedure EventLog(S: string);
function WinSockErrorCodeStr(Code: Integer): string;

implementation
uses
{$IFNDEF WIN32}
  UnixUtils,
{$ENDIF}
  SysUtils, IRCServer;

procedure Log(S: string; DiskOnly: Boolean=False);
var
  F: text;
begin
  if Copy(S, 1, 1)<>'-' then
    S:='['+TimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file WNServer.log exists
  {$I-}
  Assign(F, ExtractFilePath(ParamStr(0))+'WNServer.log');
  Append(F);
  WriteLn(F, S);
  Close(f);
  {$I+}
  if IOResult<>0 then ;

  if not DiskOnly then
    begin
    // logging to console, if it's enabled
    {$I-}
    WriteLn(S);
    {$I+}
    if IOResult<>0 then ;

    // echo to IRC OPERs
    LogToOper(S);
    end;
end;

procedure EventLog(S: string);
var
  F: text;
begin
  Log(S, True);

  if Copy(S, 1, 1)<>'-' then
    S:='['+DateTimeToStr(Now)+'] '+S;

  // logging to disk will work only if the file EventLog.log exists
  {$I-}
  Assign(F, ExtractFilePath(ParamStr(0))+'EventLog.log');
  Append(F);
  WriteLn(F, S);
  Close(f);
  {$I+}
  if IOResult<>0 then ;
end;

{$IFDEF WIN32}

{$INCLUDE WinSockCodes.inc}

function WinSockErrorCodeStr(Code: Integer): string;
var
  I: Integer;
begin
  Result:='Error #'+IntToStr(Code);
  for I:=1 to High(WinSockErrors) do
    if (WinSockErrors[I].Code=Code)or(WinSockErrors[I].Code=Code+10000) then
      Result:=WinSockErrors[I].Text;
end;

{$ELSE}

function WinSockErrorCodeStr(Code: Integer): string;
begin
  Result:=StrError(Code);
end;

{$ENDIF}

end.
