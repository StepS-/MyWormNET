MyWormNET

=========

Personal WormNET server software

Made by CyberShadow: https://github.com/CyberShadow/MyWormNET/

StepS' modification currently includes the following:

- Hiding IP addresses for everyone except self, mask configurable with the ini file
- WormNET news can now be set using the news.txt file and will be automatically adjusted by <MOTD> tags
- New multi-channel support with channel schemes and topics: can be set with the Channels.ini file, section names must be numbered
- Banlists for IPs and nicks: to ban when the server is active, use the IPBAN and NICKBAN commands on a target with an optional reason (available for ops and above)
- Users' IP addresses are no longer forced when hosting
- Changed the Game.asp content to allow The Wheat Snooper host games
- Now the verbose console logging is off by default; you may enable it again by setting it to 1 in the ini file. Disk logging will still stay verbose unless WNServer.log is deleted.
- Fixed the passworded games' padlock icons and locale typecodes
- Game list can no longer be flooded with the hosts from the same IP
- Now the hosts can only be closed either from the same IP or from 127.0.0.1
- Put IRC commands into procedures, as well as a bunch of other things
- Removed the annoying hosted/closed game notices
- LogToOper is now optional: set yourself to mode +L to enable logging (available for ops and above)
- Support for the ISON command
- Finalized the WHO command (now properly gets an one-user or one-channel list)
- Ability to kill users from the server by using the KICK or KILL command with an optional reason (available for ops and above)
- Ability to kill every non-owner from the server by using the KICKALL command (available for owners)
- Ability to prank users by using the PRANK command (available for ops and above)
- Ability to mute or unmute users by using the MUTE or UNMUTE commands (available for halfops and above)
- Ability to make an announcement (channel notice on behalf of the server) by using the ANNOUNCE command (available for halfops and above)
- Ability to sendraw an entire message by using the SENDRAW command (available for owners)
- Ability to look up any user's IP by using the IPLOOKUP command (available for ops and above), useful when no access to console
- Ability to change other users' modes when halfopped at least (not for channels right now, but needs to be executed inside a channel)
- Ability to make users invisible with the +i mode (available for ops and above)
- Support for the channel banlist
- Support for the TIME command
- Support for qaohv user modes
- Empty Username will now be adjusted
- A missing sharp in IRCChannel will now be adjusted
- Empty nicknames can no longer initiate the login procedure
- Minor improvements and fixes