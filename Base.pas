unit Base;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

const
  APPVERSION = '1.2.5.0';
  
var
  ServerHost: string;  // our hostname
  IRCPort, HTTPPort, WormNATPort: Integer;
  IRCOperPassword: string;
  IRCChannel: string;
  StealthIP: string;
  NetworkName: string;
  StartupTime: string;

procedure Log(S: string; DiskOnly: Boolean=False);
procedure EventLog(S: string);
function WinSockErrorCodeStr(Code: Integer): string;

function IRCDateTimeNow : Int64;
function TextDateTimeNow : string;

implementation
uses
{$IFNDEF WIN32}
  UnixUtils,
{$ELSE}
  Windows,
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
  Log(S);

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

{$IFDEF WIN32}

function IRCDateTimeNow : Int64;
var
  Timezone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Timezone);
  Result := Round(Now * SecsPerDay) + (Timezone.Bias * 60) - 2209176000;
end;

function TextDateTimeNow : string;
var
  Timezone : TTimeZoneInformation;
  UTCDatetime : TDateTime;
begin
  GetTimeZoneInformation(Timezone);
  UTCDatetime := Now + Timezone.Bias/1440;
  Result := DateToStr(UTCDatetime)+' '+TimeToStr(UTCDatetime)+' UTC';
end;

{$ELSE}

function IRCDateTimeNow : Int64;
begin
  Result := Round(Now * SecsPerDay) - 2209176000;
end;

function TextDateTimeNow : string;
begin
  Result := DateToStr(Now)+' '+TimeToStr(Now)+' server local time';
end;

{$ENDIF}

end.
