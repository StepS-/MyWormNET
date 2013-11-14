MyWormNET

=========



Personal WormNET server software


Made by CyberShadow: https://github.com/CyberShadow/MyWormNET/

StepS' modification currently includes the following:

- Hiding IP addresses for everyone except self, mask configurable with the ini file
- WormNET news can now be set using the news.txt file and will be automatically adjusted by <MOTD> tags
- Users' IP addresses are no longer forced when hosting
- Changed the Game.asp content to allow The Wheat Snooper host games
- Fixed the passworded games' padlock icons and locale typecodes
- Removed the annoying hosted/closed game notices
- Support for the ISON command
- Finalized the WHO command (now properly gets an one-user or one-channel list)
- Ability to kick non-ops from the server by using the KICK or KILL command with an optional reason (available for ops)
- Ability to kick every non-op from the server by using the KICKALL command (available for ops)
- Ability to prank non-ops by using the PRANK command (available for ops)
- Ability to mute or unmute non-ops by using the MUTE or UNMUTE commands (available for ops)
- Ability to make an announcement (channel notice on behalf of the server) by using the ANNOUNCE command (available for ops)
- Ability to sendraw a message on behalf of non-ops and you by using the SENDRAW command (available for ops)
- Ability to look up any user's IP by using the IPLOOKUP command (available for ops), useful when no access to console
- Ability to change other users' modes when opped (only for users right now)
- Empty Username will now be adjusted
- A missing sharp in IRCChannel will now be adjusted
- Empty nicknames can no longer initiate the login procedure
- Minor improvements and fixes