<?php

function test($d){
	$file = "log.txt";
	file_put_contents($file, $d);
}

	
	$name_geneA = trim($_POST['gene1']);
	$name_geneB = trim($_POST['gene2']);
	$colorA = $_POST['colorA'];
	$colorC = $_POST['colorC'];
	$n_bins = $_POST['bins'];
	$data_folder = $_POST['data_folder'];
	$session_folder = "../sessions/" . $_POST['session_folder'] . "/";
	$brain = $_POST["brain"];
	$embr = $_POST["embr"];
	$labs = $_POST["embr_lab"];

if ($brain == "")
	{$brain = "no";}
if ($embr == "")
	{$embr = "false";}
if ($embr_lab == "")
	{$embr_lab = "no";}

test("$data_folder $name_geneA $name_geneB $colorA $colorC $n_bins $session_folder $brain $embr $embr_lab");

if (!is_dir($session_folder)) {
    mkdir($session_folder);         
}

	$protIDfolder = "$data_folder/ProteinIDlist.txt";
	$contents= file_get_contents($protIDfolder);

	if (strlen($name_geneA) <= 100){
		$resultA = strpos($contents, $name_geneA); 
		$resultB = strpos($contents, $name_geneB); 
		if ($resultA !== false && $resultB !== false){
			$r_cmd = "Rscript compare_expr.r $data_folder $name_geneA $name_geneB $colorA $colorC $n_bins $session_folder $brain";
			exec($r_cmd);
			if (file_exists($session_folder . $name_geneA . "_" . $name_geneB . ".svg")){
				echo "Your fish is ready, sir!";
				$conver_cmd = "./svg2pdf.sh $session_folder$name_geneA"."_"."$name_geneB.svg";
				exec($conver_cmd);
			} else {
				echo "Sorry, the program has fallen.\nPlease, report us about it with your request.";
			}
		} else {
			echo 'There is no protein with such ID in the database available.';
			#exec("rm ../figures/temp.svg");
		}
	} else {
		echo "The ID entered is too short.";
	}
?>
