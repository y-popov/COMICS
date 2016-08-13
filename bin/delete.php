<?php

function test($d){
	$file = "log.txt";
	file_put_contents($file, $d);
}


	$dataset_name = $_GET["dataset"];
	$dataset_dir = "../datasets/" . $dataset_name . "/";

test($dataset_dir);
	$files = glob($dataset_dir . '*', GLOB_MARK);
	foreach ($files as $file) {
		unlink($file);
	}
	rmdir($dataset_dir);
	
	if (is_dir($dataset_dir)) {
		echo "Some error occured. Dataset was not deleted!";
	} else {
		$contents = file_get_contents("../datasets.txt");
		$lines = explode("\n", $contents);
		$include = array();
		foreach ($lines as $line) {
			if (strpos($line, $dataset_name) === FALSE) {
				$include[] = $line;
			}
		}
		$new_contents = implode("\n", $include);
		file_put_contents("../datasets.txt", $new_contents);
		echo "Dataset was deleted!";
	}
?>
