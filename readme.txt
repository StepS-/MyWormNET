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
- Ability to kick users from the server by using the KICK or KILL command with an optional reason (available for ops and above)
- Ability to kick every non-owner from the server by using the KICKALL command (available for owners)
- Ability to prank users by using the PRANK command (available for halfops and above)
- Ability to mute or unmute users by using the MUTE or UNMUTE commands (available for halfops and above)
- Ability to make an announcement (channel notice on behalf of the server) by using the ANNOUNCE command (available for halfops and above)
- Ability to sendraw a message on behalf of users and you by using the SENDRAW command (available for owners)
- Ability to look up any user's IP by using the IPLOOKUP command (available for ops and above), useful when no access to console
- Ability to change other users' modes when halfopped at least (not for channels right now, but needs to be executed inside a channel)
- Support for qaohv user modes
- Empty Username will now be adjusted
- A missing sharp in IRCChannel will now be adjusted
- Empty nicknames can no longer initiate the login procedure
- Minor improvements and fixes