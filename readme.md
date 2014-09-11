# MyWormNET 

Personal WormNET server software

Originally developed by CyberShadow: https://github.com/CyberShadow/MyWormNET/

Below you can see all the new additions and fixes made since the original release of MyWormNET:

## MyWormNET 1.4.0.0

- **Linux support** has been fixed. Builds compiled with Free Pascal on linux-based systems will no longer error out and will work correctly. For the default HTTP server port to work (required for W:A), root will be required. Currently tested on **Ubuntu 12.04 x64 and Raspberry Pi**.
- A new, redesigned **localization system**. Translations are now loadable as simple text files and are located in the "languages" directory.
  + Added translations from the community: English (StepS), French (LeTotalKiller), German (Gabberarmy), Italian (ZexorZ), Polish (PeCeTfull/ZexorZ), Portuguese (Kaleu) and Russian (StepS).
  + On Windows, setting the AppLanguage setting in the INI file to "Default" will load the default **system language**, if such a language file is present in the "languages" directory; otherwise it defaults to English. The file name needs to correspond with the English label of the language. On Linux and other systems, the default will be **English**, but can be changed manually to the preferred language.
  + Supported **encodings**: UTF-8 with signature, Unicode (LE/BE) and ANSI. All strings will be loaded according to that encoding the file uses. Escape sequences are supported. In addition, if you'd like to enter a double-byte \x character code, you can put **L** before the opening quote: in this case, \x characters will be treated as **double-byte** ones (this also allows encoding Unicode information in an ANSI file, but is not really convenient). Lightweight (ANSI-only) Windows builds will convert the console output to local codepage, normal (x86/x64 Unicode builds) output to console in Unicode. On Linux, the console output is in **UTF-8**.
- Shared lists of data such as users, channels, games and others are now **thread-safe**. This means that it won't be possible for the server and/or users to randomly crash anymore, something that was always the problem with MyWormNET. Therefore this version can be considered the first really stable version of the program.
- Added a **MinimumVersion** feature. This way you can limit the version of W:A of people who join the server (snooper users are unaffected). Note that the precise version detection only works with versions of 3.6.28.0 and above (3.6.30.0 and above for the HTTP). Otherwise, if MinimumVersion is set to a version earlier than 3.6.28.0 then the lowest possible guess will take effect.
- Added three new **white lists**. Lists have been moved to the "lists" directory and renamed. New white lists:
  + white_passauth.txt: a list of nicknames and passwords which must be used together in order for that nickname to login. If an user wants to login with that nickname, he must change the server password in his IRC client to his personal password. Note that the "Nick IP auth" list, described below, takes priority over this, but can be combined, of course. One nickname can have multiple passwords, separated with a comma, but it is not recommended, of course.
  + white_throttle.txt: a list of IPs to whom the anti-flood restrictions, including the game list, are disabled. Only enable it for services.
  + white_nickipauth.txt: a list of nicknames and IPs which limits the nick usage to those IPs only. Multiple IPs can be entered, separated with a comma, on one line, but nick-ip pairs can also be duplicated multiple times.
- Anti-flood was improved, now it will also verify the incoming data usage. Also, two new options were added: **MessageAntiFlood** and **AntiFloodFactor**, controlling the state of anti-flood and how severe it should be. Note that the general anti-flood will always be enabled, but AntiFloodFactor still applies to both. AntiFloodFactor is a floating-point value with a decimal comma. Lower value than 1,0 means lower severity, while a higher value means higher severity.
- Admins and owners can now **remove and add channels** when the server is already running. Note that this doesn't update the Channels.ini file. The command example is: /addchannel #AnythingGoes Pf,Be Channel topic bla bla.
- You can now specify your own **IRC server password**, rather than the W:A's default. Snoopers, however, most likely don't support this, so only use it for experimental purposes. The "Default" setting will use the normal password as usual. If you set it to "Random", a random pass (12 alphanumeric characters) will be generated when settings are loaded.
- You can now set the IRC operator password to **"Random"**. The "Default" option here will default to Random, generating a new password each time the settings are loaded. The random pass will contain 8 alphanumeric characters.
- The server settings can now be **reloaded** by the server owner using the RELOADSETTINGS command. All of the server settings, including the black/white lists, but not including channels and server ports, will be refreshed. Random passwords will be regenerated where they were set to Random.
- Channel topics can now be **changed** for already created channels. The username of the person who changed the topic will also be seen as such. Halfops may use this.
- Operators and above can now **change their nick**. This will be done via NICK to themselves, but other users will see them QUIT and JOIN, for W:A and snooper compatibility.
- The "WWP debug authping mode" was disabled, as it became obsolete with recent reverse-engineered algorithm from noam. In future his library might be included for allowing TwoFish authentication of WWP clients.
- Added a channel **KICK** possibility. Such users will be kicked from the channel and not killed on the server. Halfops may use this.
- The owner can now **shutdown the server** via IRC using the SHUTDOWN command. A server anouncement will be made, a second after which the server application will terminate.
- IRC **registration timeout** was added: if the user failed to sign in within 30 seconds (or change his nick), he will be dropped.
- **WormNAT is disabled** by default (port set to 0). Old [WormNAT](http://worms.thecybershadow.net/wormnat/) is obsolete, risky and probably broken some updates ago, therefore it's not recommended. Only preserved for history.
- Fixed bug introduced with 1.3.7: when signing in as operator or owner, the mode was not distributed properly across multiple channels the user was in.
- Fixed bug introduced with 1.3.7: open games were incorrectly shown as passworded (due to the WWP additions), everything is now fixed.
- Fixed bug introduced with 1.3.7: CALC would ignore the argument if it started with a whitespace.
- Fixed the "Nickname is already in use" error message.
- ASPRedirect was fixed to include the User-Agent and requester IP information. Note: untested.
- PART commands that contain messages will now be displayed properly in logs.
- Server announcements will now be included with the message. Halfops may use this.
- The "Seen" service can now be disabled, and is disabled by default.
- The SENDRAW command was replaced with SENDFROM, which executes command on behalf of an existing connected user for real (and not just sent as a raw text), available to owners as usual.
- QUIT messages will now be prefixed with "Quit: " if it was not a W:A game host/join message. If a message is empty, a simple "Quit" is inserted, as usual.
- The "server was compiled" timestamp will now be taken from the executable's linker timestamp variable on Windows. Lightweight editions (Delphi 7, etc.) use the previous method as they didn't set the timestamp properly yet.
- If the IRC server is disabled, all HTTP activity will be seen in console even without VerboseConsoleLogging.
- Changed modes b and L: they are now Q and s for muting and logging, respectively.
- Command for becoming owner can now be used with SOPER and OWNER aliases.
- USERS is now an associate part of LUSERS that may be called separately.
- EXPECT will be disabled if WormNAT is disabled. Both are probably broken, anyway.
- Username and nickname will no longer be empty by default (but their stubs will only be used where needed, preventing previous bugs).
- The LogToOper feature will now work for admins and owners with mode +s enabled.
- ERRORs sent by server will now be traditionally formatted, except for pranks.
- User kills and server kills have been separated.
- news.txt was renamed to news.xml for consistency and syntax highlighting in advanced text editors.
- Server lists were updated to the more contemporary ones.
- All server units have been documented to describe their purpose.
- A lot of other fixes and internal improvements.

## MyWormNET 1.3.7.0

- **(!)** Fixed QUIT message for kicked/banned users
- Fixed IRC password check
- WWP clients can no longer be exploited with a WWP flag bug on this server
 + They will also have a "WWP" username now, and their hosts will be prefixed with [WWP]
- User-Agent and version check to alternate content between different W:A and WWP versions, as well as other programs
- Support for the PART reason text
- Ability to perform certain text operations with the CALC command (use CALC help for the list) (available for voiced and above)
- Ability to use arbitrary asp/php pages rather than the predefined ones
 + Can be enabled by setting it to 1 in the ini file
 + Will work if file with the full request path exists
- Quit message will now be shown in the Event/Console log alongside "disconnected".

## MyWormNET 1.3.6.0

- WWP support
 + Partial: game list needs to be encrypted in order to be viewable by WWP clients.
 + Support for both ASP and PHP pages, WWP default is currently PHP
 + Both W:A and WWP people can join and chat
- Support for the INFO command
 + Now the credits are moved to there and can be accessed anytime, not only during log in
 + Also shows creation time and when went online
- WWP "debug mode" for the AUTHPING/AUTHPONG commands
 + NOTE: NOT FOR NORMAL USAGE. Will turn the IRC server into a test server that will only send AUTHPING's, then accept and memorize AUTHPONG's and then disconnect the client. The best practice is to change the HTTP/IRC ports for this mode if also running another (normal) copy of the server (WWP supports arbitrary IRC ports, HTTP port can be set in ServerList.htm)
 + Can be enabled by setting "ForceAuthping" to 1 in the ini file. Edit authping.txt with the Secret, Challenge and Answer strings, each on a new line. The Answer (last string) will be encrypted by WWP when connecting to a test server and then saved to authpong.txt
 + After the WWP client's connection to a test server, the AUTHPONG command will be waiting for you in the authpong.txt file. Please see the TUS forum to exactly know what to do: the operation may be not so simple and all of the steps have to be followed carefully
- Now the user messages will use a converted W:A codepage for logs
- Support for the TOPIC command which can now be called anytime
- Support for the LUSERS command which can now be called anytime
 + Now also includes max user count, unknown connections and a total number of connections
- Hosting with special characters (pre-converted to URI) in titles and nicknames is now supported
- Invalid nicks such as "server", "HostingBuddy", "admin" and alike won't be able to connect to the server now
- Default starting Game ID is now 1000000, to prevent issues caused by a W:A bug when switching between UK WormNET server and an external one. Should be fixed in a future W:A update.
- Optimized verbose logging to reduce disk space waste
- Broadcasting to users is now done more properly
- Various other improvements and fixes

## MyWormNET 1.3.5.0

- **(!)** Fixed a major issue with incoming data buffer reset leading to rare failures of user LogIns and HTTP requests
- Message anti-flood
   + Points-based, works depending on message size and send rate, rate cooldown is 2 seconds
- Support for the SEEN command, also available with !seen
   + Will retrieve the seen status from quit of a given user with time since the server start
   + Not visible to others when used as !seen
- A notification will be thrown when attempting to use the !host or !phost commands
- Can type commands beginning with $, allowing to use server commands from W:A
   + Such commands are not visible to anyone after being sent
- Support for the AWAY command
- Change the game ID counter with the FORCEGAMEID command (available for ops and above)
- Various other improvements and fixes

## Versions before 1.3.5.0

- Hiding IP addresses for everyone except self, mask configurable with the ini file
   + Default is "no.address.for.you"
- WormNET news can now be set using the news.txt file and will be automatically adjusted by <MOTD> tags
   + A sample news.txt file is provided
- New multi-channel support with channel schemes and topics: can be set with the Channels.ini file, section names must be numbered
   + Section names go as follows: [1], [2], [3], ...
   + If missing Channels.ini file or its contents, a default #AnythingGoes channel will be created.
- Banlists for IPs and nicks: to ban and unban nick or ip when the server is active, use the PERMABAN and REMOVEBAN commands on a target with an optional reason (available for ops and above)
   + Connections from banned IPs to any of the server ports will be rejected.
   + Banned nicks are not case-sensitive.
   + Banned nicks will be killed with a notification when trying to log in.
- Localization support: translate MyWormNET into any language thanks to the dedicated resource strings.
   + Russian localization by StepS
   + Polish localization by PeCeT_full
- Support for x64 and various different Delphi compilers (from Delphi 7 to XE5 at least)
- Support for Unicode compiler
- Users' IP addresses are no longer forced when hosting
   + The hosting address will no longer be overridden by the socket's address, allowing for WormNAT2 and custom address hosting.
- Changed the Game.asp content to allow The Wheat Snooper host games
   + Wheat Snooper looks for the "Object moved" substring and then for some stupid reason fetches the GameList URL from there. Without it, it refuses to host.
- Now the verbose console logging is off by default
   + You may enable it again by setting it to 1 in the ini file
   + Disk logging to WNServer.log will still stay verbose unless that file is deleted
- Fixed the passworded games' padlock icons and locale typecodes
   + Properly show if the game is open or not, as well as the country locale code if the flag is not in 0-48 range
- Game list can no longer be flooded with the hosts from the same IP
   + If a game was already in the list from the request IP, it will be overridden by the new one instead of stacking.
- Now the hosts can only be closed either from the same IP or from 127.0.0.1
   + Prevent outsiders from closing other people's hosts
- Put IRC commands into TUser procedures, as well as a bunch of other routine things
   + For interoperability and proper organization
- Removed the annoying hosted/closed game notices
   + No more channel notices when a game is hosted or closed
- LogToOper is now disabled by default
   + Set yourself to mode +L to enable logging (available for ops and above)
- Support for the ISON command
- Finalized the WHO command
   + Now properly gets an one-user or one-channel list, as well as 'o' list
- Support for the WHOIS command
   + Reveals IP of a WHOIS'd user to operators and above, making the IPLOOKUP command obsolete (it's still left as an alias, though).
   + When WHOIS'ing themselves, users can also see their own IP.
- Support for the MOTD command which can now be called anytime
- Ability to kill users from the server by using the KICK or KILL command with an optional reason (available for ops and above)
- Ability to kill every non-owner from the server by using the KICKALL command (available for owners)
- Ability to prank users by using the PRANK command (available for ops and above)
   + Will send IRC ERROR with a given text to an user, forcing him to quit if the client is W:A.
- Ability to mute or unmute users by using the MUTE or UNMUTE commands (available for halfops and above)
   + Will set the +b mode and add the user to channel banlist (but won't prevent him from joining the channels).
   + Muted users can't perform public and private messaging.
- Ability to make an announcement by using the ANNOUNCE command (available for halfops and above)
   + Server will spread a NOTICE with its nickname to users on channels that contains user-defined announcement.
- Ability to sendraw an entire message by using the SENDRAW command (available for owners)
   + Will spread an entirely raw IRC message with any content to everyone on the server.
- Ability to change other users' modes when halfopped at least
   + Halfops can only change modes b and v.
   + Modes only apply to users, but as usual, the MODE command has to contain a target channel.
- Ability to make users invisible with the +i mode (available for ops and above)
- Support for the channel banlist
   + Mode +b <channel> will show who is currently muted.
- Support for the TIME command
   + Will show the server's local time in response (and provide UTC offset on Windows).
- Support for qaohv user modes
   + Owner (superadmin) is mode +q, Admin is mode +a, etc.
   + All commands except sendraw are available to normal ops, so most users will find this useless unless they want more defined privileges (as in, ops can act upon ops, unlike admins)
   + Use TAKEOWN with the same operator password to make yourself a server owner.
- Empty Username will now be adjusted
- Missing sharps in IRC channel names will now be adjusted
- Empty nicknames can no longer initiate the login procedure
- Minor improvements and fixes
