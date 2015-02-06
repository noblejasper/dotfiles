<?php
$filename = $argv[1];
$str = file_get_contents($filename);
$str = strip_tags($str);

$patterns = array(
	"/^ */",
	"/{#[^#]*#}/i",
	"/{%.*?%}/i",
	"/^ */",
	"/{{ line\(\) }}/",
	"/{{ 3\|space\|safe }}/",
	);
$replacements = array(
	"",
	"",
	"",
	"",
	"",
	"",
	"",
	);

$result = "";
foreach (split("\n", $str) as $key => $line) {
	$l = preg_replace($patterns, $replacements, $line);
	if(strlen($l) > 0) {
		$result .= $l . "\n";
	}
}

$ml_patterns = array(
	"/{#[^#]*#}/i",
	);
$ml_replacements = array(
	"",
	);
$result = preg_replace($patterns, $replacements, $result);

$fp = fopen($filename, "w");
@fwrite( $fp, $result, strlen($result) );
fclose($fp);

print "done " . $filename . "\n";
?>