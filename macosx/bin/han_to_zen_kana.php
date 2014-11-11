<?php
$filename = $argv[1];
$str = file_get_contents($filename);
$str = mb_convert_kana($str, 'KV','utf8');

$fp = fopen($filename, "w");
@fwrite( $fp, $str, strlen($str) );
fclose($fp);

print "done " . $filename . "\n";
?>