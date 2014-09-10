unit Localization;

{-------------------------------------------------------------------------------
| MyWormNET localization unit.
| (C) StepS - 2014
|-------------------------------------------------------------------------------
| FEATURES:
|
| - Parse the language files located in the 'languages' directory
| - Manage the loc strings and IDs assisted with the Data unit
| - Autodetect the system language when set to 'default' on MS Windows
| - Comments in language files start with '//'
-------------------------------------------------------------------------------}

interface

{$IFDEF FPC}
{$mode DELPHI}
type PString = ^string;
{$ENDIF}

function GetLocalLanguageName: String;
procedure ParseLocFile(FN: String);
function GetSIDByText(S: string): PString;
function GetLocalizations(Loc: String): Boolean;

var
  L_START                      : string = 'Starting MyWormNET version %s.';
  L_ERROR                      : string = 'Error';
  L_ERROR_WITH                 : string = 'Error with';
  L_ERROR_SELECT               : string = 'select() error';
  L_ERROR_CLIENT_SELECT        : string = 'Client select() error';
  L_ERROR_CLIENT_CONNECTION    : string = 'Client connection error';
  L_ERROR_SERVER_SELECT        : string = 'Server select() error';
  L_ERROR_SERVER_CONNECTION    : string = 'Server connection error';
  L_ERROR_BIND                 : string = 'Error when trying to bind to port %u (%s).';
  L_ERROR_FAILED               : string = 'Failed';
  L_CONNECTION_CLOSING_LINK    : string = 'Closing link to';
  L_CONNECTION_REJECTED        : string = 'Rejected request from banned IP %s to port %u.';
  L_CONNECTION_ERROR           : string = 'Connection error';
  L_CONNECTION_ESTABLISHED     : string = 'Connection established from %s';
  L_SERVICE_LISTENING          : string = 'Listening on port %u.';
  L_IRC_ADMIN_LOGIN            : string = '%s has registered as an %s.';
  L_IRC_ADMIN_OPER             : string = 'Operator';
  L_IRC_ADMIN_OWNER            : string = 'Owner';
  L_IRC_CHANNEL_ADDED          : string = 'Channel %s has been added.';
  L_IRC_CHANNEL_ALREADY        : string = 'Channel %s has already been created: ignored.';
  L_IRC_CHANNEL_EMPTY          : string = 'No channels found in the Channels.ini file: default channel will be created.';
  L_IRC_CHANNEL_NOT_FOUND      : string = 'Could not find the Channels.ini file: default channel will be created.';
  L_IRC_CHANNEL_REMOVED        : string = 'Channel %s has been removed.';
  L_IRC_CONNECTING             : string = '%s is connecting to the IRC...';
  L_IRC_EXPECT                 : string = 'Received EXPECT command from %s for %s';
  L_IRC_JOIN                   : string = '%s has joined %s.';
  L_IRC_PART                   : string = '%s has left %s.';
  L_IRC_PART_EX                : string = '%s has left %s (%s).';
  L_IRC_RANDOM_PASS            : string = 'Random IRC password: %s';
  L_IRC_RANDOM_OPERPASS        : string = 'Random operator password: %s';
  L_IRC_TYPE_NICKNAME          : string = 'nickname';
  L_IRC_TYPE_IP                : string = 'IP';
  L_IRC_ACTION_FAILBAN         : string = '%s has attempted to ban %s without privileges.';
  L_IRC_ACTION_BAN             : string = '%s has banned %s, reason: %s';
  L_IRC_ACTION_UNBAN           : string = '%s has unbanned %s, reason: %s';
  L_IRC_ACTION_PRANK           : string = '%s has pranked %s, description: %s';
  L_IRC_ACTION_KICK            : string = '%s has kicked %s from %s, reason: %s';
  L_IRC_ACTION_KICKALL         : string = '%s has killed everyone on the server.';
  L_IRC_ACTION_KILL            : string = '%s has killed %s, reason: %s';
  L_IRC_ACTION_MUTE            : string = '%s has muted %s.';
  L_IRC_ACTION_UNMUTE          : string = '%s has unmuted %s.';
  L_IRC_ACTION_HIDE            : string = '%s has made %s invisible.';
  L_IRC_ACTION_UNHIDE          : string = '%s has made %s visible again.';
  L_IRC_ACTION_ANNOUNCE        : string = '%s has made a global announcement: "%s".';
  L_IRC_ACTION_MODE            : string = '%s has set mode %s to %s.';
  L_IRC_ACTION_NICK_FAIL       : string = '%s has attempted to change his nick to "%s".';
  L_IRC_ACTION_NICK_SUCCESS    : string = '%s has changed his nick to "%s".';
  L_IRC_ACTION_AWAY            : string = '%s has set an away message: "%s".';
  L_IRC_ACTION_BACK            : string = '%s has returned from an away state.';
  L_IRC_ACTION_FORCEGAMEID     : string = '%s has forced the GameCounter to %s.';
  L_IRC_ACTION_TOPIC           : string = '%s has changed the %s channel''s topic to "%s".';
  L_IRC_ACTION_RELOADSETTINGS  : string = '%s has reloaded the server settings.';
  L_IRC_ACTION_ADDCHANNEL      : string = '%s is creating a channel %s...';
  L_IRC_ACTION_REMOVECHANNEL   : string = '%s is removing the %s channel...';
  L_IRC_ACTION_SHUTDOWN        : string = '%s is shutting the server down...';
  L_IRC_LOG_RESPONSE           : string = 'Sent %s command response to %s';
  L_IRC_LOG_WELCOME            : string = 'Sent welcome text to %s';
  L_IRC_LOGGED_IN              : string = '%s (%s) has logged in.';
  L_IRC_HALT_BANNED_NICK       : string = '%s (%s) has been halted due to his nick being banned.';
  L_IRC_DISCONNECTED           : string = '%s (%s) has disconnected (%s).';
  L_IRC_DISCONNECTED_SILENT    : string = '%s (%s) has disconnected.';
  L_IRC_DISCONNECTED_UNKNOWN   : string = '%s has disconnected, passing auth.';
  L_IRC_DISCONNECTED_UNKAUTH   : string = '%s has disconnected without passing auth.';
  L_HTTP_FILE_SENDING          : string = 'Sending file';
  L_GAME_FAIL_NONGAMING        : string = '%s (%s) has attempted to create a game %s with address %s on a non-gaming channel %s';
  L_GAME_FAIL_SABOTAGE         : string = '%s has attempted to close %s''s game (ID %s) which was hosted from IP %s';
  L_GAME_FAIL_NONEXISTENT_CHAN : string = '%s (%s) has attempted to create a game %s with address %s on a nonexistent channel %s';
  L_GAME_FAIL_NONEXISTENT_GAME : string = '%s has attempted to close a nonexistent game (ID %s)';
  L_GAME_CLOSED_ANTIFLOOD      : string = '%s''s game "%s" has closed to prevent flood from IP %s';
  L_GAME_CLOSED_GRACEFULLY     : string = '%s''s game "%s" has closed.';
  L_GAME_CLOSED_TIMEOUT        : string = '%s''s game "%s" has closed due to time limit.';
  L_GAME_CREATED_INFO          : string = '%s (%s) has created a game "%s" on %s: %s';
  L_WORMNAT_START_LINK         : string = 'Initializing link between';
  L_WORMNAT_LINK_ERROR         : string = 'Error in link with';
  L_WORMNAT_UNEXPECTED         : string = 'Unexpected connection from';

implementation
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Data;

function GetLocalizations(Loc: String): Boolean;
var
  LocFilePath, LocaleName: String;
begin
  Result:=true;
  if TextMatch(Loc, 'default') then LocaleName := GetLocalLanguageName
  else LocaleName := Loc;
  LocFilePath:='languages'+PathDelim+LocaleName+'.txt';
  if FileExists(LocFilePath) then
    ParseLocFile(LocFilePath)
  else if not TextMatch(Loc, 'default') then
    Result:=false;
end;

function GetLocalLanguageName: String;
{$IFDEF MSWINDOWS}
var
  LocaleName: array[0..255] of AnsiChar;
begin
  if GetLocaleInfoA(GetSystemDefaultLCID,LOCALE_SENGLANGUAGE,LocaleName,256) <> 0 then
    Result:=String(LocaleName)
  else
    Result:='English';
{$ELSE}
begin
  Result:='English';
{$ENDIF}
end;

procedure ParseLocFile(FN: String);
var
  Buf, Line: string;
  PID: PString;
begin
  Buf:=GetTextFile(FN);
  while GetLine(Buf, Line) do
  begin
    if Pos('//', TrimLeft(Line+'//')) = 1 then continue;
    PID:=GetSIDByText(GetLocIDFromString(Line));
    if PID <> nil then
      PID^:=GetLocValueFromString(Line);
  end;
end;

function GetSIDByText(S: string): PString;
begin
  Result:=nil;
  if S = '' then
    Result := nil
  else if S =   'START' then
    Result := @L_START
  else if S =   'ERROR' then
    Result := @L_ERROR
  else if S =   'ERROR_WITH' then
    Result := @L_ERROR_WITH
  else if S =   'ERROR_SELECT' then
    Result := @L_ERROR_SELECT
  else if S =   'ERROR_CLIENT_SELECT' then
    Result := @L_ERROR_CLIENT_SELECT
  else if S =   'ERROR_CLIENT_CONNECTION' then
    Result := @L_ERROR_CLIENT_CONNECTION
  else if S =   'ERROR_SERVER_SELECT' then
    Result := @L_ERROR_SERVER_SELECT
  else if S =   'ERROR_SERVER_CONNECTION' then
    Result := @L_ERROR_SERVER_CONNECTION
  else if S =   'ERROR_BIND' then
    Result := @L_ERROR_BIND
  else if S =   'ERROR_FAILED' then
    Result := @L_ERROR_FAILED
  else if S =   'CONNECTION_CLOSING_LINK' then
    Result := @L_CONNECTION_CLOSING_LINK
  else if S =   'CONNECTION_REJECTED' then
    Result := @L_CONNECTION_REJECTED
  else if S =   'CONNECTION_ERROR' then
    Result := @L_CONNECTION_ERROR
  else if S =   'CONNECTION_ESTABLISHED' then
    Result := @L_CONNECTION_ESTABLISHED
  else if S =   'SERVICE_LISTENING' then
    Result := @L_SERVICE_LISTENING
  else if S =   'IRC_ADMIN_LOGIN' then
    Result := @L_IRC_ADMIN_LOGIN
  else if S =   'IRC_ADMIN_OPER' then
    Result := @L_IRC_ADMIN_OPER
  else if S =   'IRC_ADMIN_OWNER' then
    Result := @L_IRC_ADMIN_OWNER
  else if S =   'IRC_CHANNEL_ADDED' then
    Result := @L_IRC_CHANNEL_ADDED
  else if S =   'IRC_CHANNEL_ALREADY' then
    Result := @L_IRC_CHANNEL_ALREADY
  else if S =   'IRC_CHANNEL_EMPTY' then
    Result := @L_IRC_CHANNEL_EMPTY
  else if S =   'IRC_CHANNEL_NOT_FOUND' then
    Result := @L_IRC_CHANNEL_NOT_FOUND
  else if S =   'IRC_CHANNEL_REMOVED' then
    Result := @L_IRC_CHANNEL_REMOVED
  else if S =   'IRC_CONNECTING' then
    Result := @L_IRC_CONNECTING
  else if S =   'IRC_EXPECT' then
    Result := @L_IRC_EXPECT
  else if S =   'IRC_JOIN' then
    Result := @L_IRC_JOIN
  else if S =   'IRC_PART' then
    Result := @L_IRC_PART
  else if S =   'IRC_PART_EX' then
    Result := @L_IRC_PART_EX
  else if S =   'IRC_RANDOM_PASS' then
    Result := @L_IRC_RANDOM_PASS
  else if S =   'IRC_RANDOM_OPERPASS' then
    Result := @L_IRC_RANDOM_OPERPASS
  else if S =   'IRC_TYPE_NICKNAME' then
    Result := @L_IRC_TYPE_NICKNAME
  else if S =   'IRC_TYPE_IP' then
    Result := @L_IRC_TYPE_IP
  else if S =   'IRC_ACTION_FAILBAN' then
    Result := @L_IRC_ACTION_FAILBAN
  else if S =   'IRC_ACTION_BAN' then
    Result := @L_IRC_ACTION_BAN
  else if S =   'IRC_ACTION_UNBAN' then
    Result := @L_IRC_ACTION_UNBAN
  else if S =   'IRC_ACTION_PRANK' then
    Result := @L_IRC_ACTION_PRANK
  else if S =   'IRC_ACTION_KICK' then
    Result := @L_IRC_ACTION_KICK
  else if S =   'IRC_ACTION_KICKALL' then
    Result := @L_IRC_ACTION_KICKALL
  else if S =   'IRC_ACTION_KILL' then
    Result := @L_IRC_ACTION_KILL
  else if S =   'IRC_ACTION_MUTE' then
    Result := @L_IRC_ACTION_MUTE
  else if S =   'IRC_ACTION_UNMUTE' then
    Result := @L_IRC_ACTION_UNMUTE
  else if S =   'IRC_ACTION_HIDE' then
    Result := @L_IRC_ACTION_HIDE
  else if S =   'IRC_ACTION_UNHIDE' then
    Result := @L_IRC_ACTION_UNHIDE
  else if S =   'IRC_ACTION_ANNOUNCE' then
    Result := @L_IRC_ACTION_ANNOUNCE
  else if S =   'IRC_ACTION_MODE' then
    Result := @L_IRC_ACTION_MODE
  else if S =   'IRC_ACTION_NICK_FAIL' then
    Result := @L_IRC_ACTION_NICK_FAIL
  else if S =   'IRC_ACTION_NICK_SUCCESS' then
    Result := @L_IRC_ACTION_NICK_SUCCESS
  else if S =   'IRC_ACTION_AWAY' then
    Result := @L_IRC_ACTION_AWAY
  else if S =   'IRC_ACTION_BACK' then
    Result := @L_IRC_ACTION_BACK
  else if S =   'IRC_ACTION_FORCEGAMEID' then
    Result := @L_IRC_ACTION_FORCEGAMEID
  else if S =   'IRC_ACTION_TOPIC' then
    Result := @L_IRC_ACTION_TOPIC
  else if S =   'IRC_ACTION_RELOADSETTINGS' then
    Result := @L_IRC_ACTION_RELOADSETTINGS
  else if S =   'IRC_ACTION_ADDCHANNEL' then
    Result := @L_IRC_ACTION_ADDCHANNEL
  else if S =   'IRC_ACTION_REMOVECHANNEL' then
    Result := @L_IRC_ACTION_REMOVECHANNEL
  else if S =   'IRC_ACTION_SHUTDOWN' then
    Result := @L_IRC_ACTION_SHUTDOWN
  else if S =   'IRC_LOG_RESPONSE' then
    Result := @L_IRC_LOG_RESPONSE
  else if S =   'IRC_LOG_WELCOME' then
    Result := @L_IRC_LOG_WELCOME
  else if S =   'IRC_LOGGED_IN' then
    Result := @L_IRC_LOGGED_IN
  else if S =   'IRC_HALT_BANNED_NICK' then
    Result := @L_IRC_HALT_BANNED_NICK
  else if S =   'IRC_DISCONNECTED' then
    Result := @L_IRC_DISCONNECTED
  else if S =   'IRC_DISCONNECTED_SILENT' then
    Result := @L_IRC_DISCONNECTED_SILENT
  else if S =   'IRC_DISCONNECTED_UNKNOWN' then
    Result := @L_IRC_DISCONNECTED_UNKNOWN
  else if S =   'IRC_DISCONNECTED_UNKAUTH' then
    Result := @L_IRC_DISCONNECTED_UNKAUTH
  else if S =   'HTTP_FILE_SENDING' then
    Result := @L_HTTP_FILE_SENDING
  else if S =   'GAME_FAIL_NONGAMING' then
    Result := @L_GAME_FAIL_NONGAMING
  else if S =   'GAME_FAIL_SABOTAGE' then
    Result := @L_GAME_FAIL_SABOTAGE
  else if S =   'GAME_FAIL_NONEXISTENT_CHAN' then
    Result := @L_GAME_FAIL_NONEXISTENT_CHAN
  else if S =   'GAME_FAIL_NONEXISTENT_GAME' then
    Result := @L_GAME_FAIL_NONEXISTENT_GAME
  else if S =   'GAME_CLOSED_ANTIFLOOD' then
    Result := @L_GAME_CLOSED_ANTIFLOOD
  else if S =   'GAME_CLOSED_GRACEFULLY' then
    Result := @L_GAME_CLOSED_GRACEFULLY
  else if S =   'GAME_CLOSED_TIMEOUT' then
    Result := @L_GAME_CLOSED_TIMEOUT
  else if S =   'GAME_CREATED_INFO' then
    Result := @L_GAME_CREATED_INFO
  else if S =   'WORMNAT_START_LINK' then
    Result := @L_WORMNAT_START_LINK
  else if S =   'WORMNAT_LINK_ERROR' then
    Result := @L_WORMNAT_LINK_ERROR
  else if S =   'WORMNAT_UNEXPECTED' then
    Result := @L_WORMNAT_UNEXPECTED;
end;

end.

