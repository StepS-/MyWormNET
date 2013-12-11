MyWormNET

=========

Personal WormNET server software

Made by CyberShadow: https://github.com/CyberShadow/MyWormNET/

StepS' modification currently includes the following:

• New multi-channel support with channel schemes and topics: can be set with the Channels.ini file, section names must be numbered
   - Section names go as follows: [1], [2], [3], ...
   - If missing Channels.ini file or its contents, a default #AnythingGoes channel will be created.
• Message anti-flood
   - Points-based, works depending on message size and send rate, rate cooldown is 2 seconds
• Banlists for IPs and nicks: to ban and unban nick or ip when the server is active, use the PERMABAN and REMOVEBAN commands on a target with an optional reason (available for ops and above)
   - Connections from banned IPs to any of the server ports will be rejected.
   - Banned nicks are not case-sensitive.
   - Banned nicks will be killed with a notification when trying to log in.
• Localization support: translate MyWormNET into any language thanks to the dedicated resource strings
   - Russian localization by StepS
   - Polish localization by PeCeT_full
   - German (Austria) localization by Gabberarmy
• WWP support
   - Partial: game list needs to be encrypted in order to be viewable by WWP clients.
   - Support for both ASP and PHP pages, WWP default is currently PHP
• WormNET news can now be set using the news.txt file and will be automatically adjusted by <MOTD> tags
   - A sample news.txt file is provided
• Hiding IP addresses for everyone except self, mask configurable with the ini file
   - Default is "no.address.for.you"
• Support for x64 and various different Delphi compilers (from Delphi 7 to XE5 at least)
• Support for Unicode compiler
• Support for the INFO command
   - Now the credits are moved to there and can be accessed anytime, not only during log in
   - Also shows creation time and when went online
• Support for the SEEN command, also available with !seen
   - Will retrieve the seen status from quit of a given user with time since the server start
   - Will not be swallowed by the server unlike !host and !phost
• WWP "debug mode" for the AUTHPING/AUTHPONG commands
   - NOTE: NOT FOR NORMAL USAGE. Will turn the IRC server into a test server that will only send AUTHPINGs, then accept and memorize AUTHPONG's and then disconnect the client
   - NOTE: the best practice is to change the HTTP/IRC ports for this mode if also running another (normal) copy of the server (WWP supports arbitrary IRC ports, HTTP port can be set in ServerList.htm)
   - Can be enabled by setting "ForceAuthpong" to 1 in the ini file
   - Edit authping.txt with the Secret, Challenge and Answer strings, each on a new line
   - The Answer (last string) will be encrypted by WWP when connecting to a test server and then saved to authpong.txt
   - After the WWP client's connection to a test server, the AUTHPONG command will be waiting for you in the authpong.txt file
   - Please see http://tus-wa.com/?topic=22731 to exactly know what to do: the operation may be not so simple and all of the steps have to be followed carefully
• A notification will be thrown when attempting to use the !host or !phost commands
• Users' IP addresses are no longer forced when hosting
   - The hosting address will no longer be overridden by the socket's address, allowing for WormNAT2 and custom address hosting.
• Changed the Game.asp content to allow The Wheat Snooper host games
   - Wheat Snooper looks for the "Object moved" substring and then for some stupid reason fetches the GameList URL from there. Without it, it refuses to host.
• Now the verbose console logging is off by default
   - You may enable it again by setting it to 1 in the ini file
   - Disk logging to WNServer.log will still stay verbose unless that file is deleted
• Now the user messages will use a converted W:A codepage for logs
• Can type commands beginning with $, allowing to use server commands from W:A
   - Such commands are not visible to anyone after being sent
• Fixed the passworded games' padlock icons and locale typecodes
   - Properly show if the game is open or not, as well as the country locale code if the flag is not in 0-48 range
• Game list can no longer be flooded with the hosts from the same IP
   - If a game was already in the list from the request IP, it will be overridden by the new one instead of stacking.
• Now the hosts can only be closed either from the same IP or from 127.0.0.1
   - Prevent outsiders from closing other people's hosts
• Put IRC commands into TUser procedures, as well as a bunch of other routine things
   - For interoperability and proper organization
• Removed the annoying hosted/closed game notices
   - No more channel notices when a game is hosted or closed
• LogToOper is now disabled by default
   - Set yourself to mode +L to enable logging (available for ops and above)
• Support for the ISON command       
• Support for the TOPIC command which can now be called anytime
• Support for the LUSERS command which can now be called anytime
   - Now also includes max user count, unknown connections and a total number of connections
• Support for the AWAY command
• Finalized the WHO command
   - Now properly gets an one-user or one-channel list, as well as 'o' list
• Support for the WHOIS command
   - Reveals IP of a WHOIS'd user to operators and above, making the IPLOOKUP command obsolete (it's still left as an alias, though).
   - When WHOIS'ing themselves, users can also see their own IP.
• Support for the MOTD command which can now be called anytime
• Hosting with special characters (pre-converted to URI) in titles and nicknames is now supported
• Ability to kill users from the server by using the KICK or KILL command with an optional reason (available for ops and above)
• Ability to kill every non-owner from the server by using the KICKALL command (available for owners)
• Ability to prank users by using the PRANK command (available for ops and above)
   - Will send IRC ERROR with a given text to an user, forcing him to quit if the client is W:A.
• Ability to mute or unmute users by using the MUTE or UNMUTE commands (available for halfops and above)
   - Will set the +b mode and add the user to channel banlist (but won't prevent him from joining the channels).
   - Muted users can't perform public and private messaging.
• Ability to make an announcement by using the ANNOUNCE command (available for halfops and above)
   - Server will spread a NOTICE with its nickname to users on channels that contains user-defined announcement.
• Ability to sendraw an entire message by using the SENDRAW command (available for owners)
   - Will spread an entirely raw IRC message with any content to everyone on the server.
• Ability to change other users' modes when halfopped at least
   - Halfops can only change modes b and v.
   - Modes only apply to users, but as usual, the MODE command has to contain a target channel.
• Ability to make users invisible with the +i mode (available for ops and above)
• Support for the channel banlist
   - Mode +b <channel> will show who is currently muted.
• Support for the TIME command
   - Will show the server's local time in response (and provide UTC offset on Windows).
• Support for qaohv user modes
   - Owner (superadmin) is mode +q, Admin is mode +a, etc.
   - All commands except sendraw are available to normal ops, so most users will find this useless unless they want more defined privileges (as in, ops can act upon ops, unlike admins)
   - Use TAKEOWN with the same operator password to make yourself a server owner.
• Invalid nicks such as "server", "HostingBuddy", "admin" and alike won't be able to connect to the server now
• Default starting Game ID is now 1000000, to prevent issues caused by a W:A bug when switching between UK WormNET server and an external one. Should be fixed in a future W:A update.
• Change the game ID counter with the FORCEGAMEID command (available for ops and above)
• Empty Username will now be adjusted
• Optimized verbose logging to reduce disk space waste
• Broadcasting to users is now done more properly
• Missing sharps in IRC channel names will now be adjusted
• Empty nicknames can no longer initiate the login procedure
• Minor improvements and fixes