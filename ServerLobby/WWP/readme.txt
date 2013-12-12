This is the very latest ServerList.htm for WWP (as of 12/12/2013) that contains the contemporary server list, including Team17's UK server, StepS' W:A/WWP cross-server, and two links for you to test your newly created server. The default one on port 80 is for normal usage, while the other one on port 81 is made with the ForceAuthping mode enabled in mind. WWP supports arbitrary IRC ports, so you can just start a second MyWormNET server with any other IRC port and HTTP port 81, so that it doesn't interfere with your main setup (of course it's advised to be done in a different directory).
How to use the forceauthping debug mode:
- Start your server with HTTP port 81 and any IRC port
- Edit the authping.txt file with the information you have: Secret, Challenge and Answer (the latter one can be gotten from RequestAuth.php of Team17's WormNET, see the link below for more info)
- Now connect to your debug server from WWP. A confirmation window will be produced and you'll disconnect.
- Open the authpong.txt file now and get the result of the encryption (last parameter in the command). Provided everything was done correctly, this is the encrypted variant of a hopefully correctly acquired answer that should let you connect to the Team17's WWP WormNET UK externally.

Please see this thread for the step-by-step guide on this "test mode":
http://tus-wa.com/?topic=22731
