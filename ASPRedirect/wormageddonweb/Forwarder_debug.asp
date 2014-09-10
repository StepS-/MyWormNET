<?php 

$URI = $_SERVER["REQUEST_URI"];
if($URI=="")
  $URI = $_ENV["REQUEST_URI"];
if($URI=="")
{
  header("Error: : Can't get REQUEST_URI !");
  die("Can't get REQUEST_URI !");
} 

error_log("Forwarder: Trying to connect to MyWormNET.");

$fp = fsockopen("localhost", 17080, $errno, $errstr, 5);

if (!$fp) 
{
  echo "$errstr ($errno)<br />\n";
  header("Error: : Apache can't connect to WormNET server - $errstr ($errno)");
} 
else 
{
  error_log("Forwarder: Connection established.");

  $USERAGENT = $_SERVER["HTTP_USER_AGENT"];
  $USERIP    = $_SERVER["REMOTE_ADDR"];
  $out = "GET $URI HTTP/1.0\r\nUser-Agent: $USERAGENT\r\nRequester: $USERIP\r\n\r\n";

  fwrite($fp, $out);
  error_log("Forwarder: Sent request to MyWormNET.");

  $response=fgets($fp);  // skip HTTP response
  error_log("Forwarder: HTTP response ($response) skipped...");

  $body = false;

  while (!feof($fp)) 
  {
    $s = fgets($fp);
    error_log("Forwarder: Received line [1] ($s).");
    $s = substr($s, 0, strlen($s)-2);
    error_log("Forwarder: Received line [2] ($s).");
    if(!$body)
      if($s=="")
      {
        $body=true;
        error_log("Forwarder: received empty line; switching to BODY mode.");
      }
      else
      {
        header($s);
        error_log("Forwarder: sent header: $s.");
      }
    else
    {
      print("$s\r\n");
      error_log("Forwarder: sent body line: $s.");
    }
  }
  error_log("Forwarder: Done pumping data, closing connection.");
  flush();
  fclose($fp);
}
  
?>
