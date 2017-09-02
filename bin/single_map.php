<?php

function test($d){
	$file = "log.txt";
	file_put_contents($file, $d);
}

	
	$name_gene = trim($_POST['gene']);
	$colorA = $_POST['colorA'];
	$colorB = $_POST['colorB'];
	$colorC = $_POST['colorC'];
	$n_bins = $_POST['bins'];
	$scale = $_POST['scale'];
	$gradient = $_POST['gradient'];
	$data_folder = $_POST['data_folder'];
	$session_folder = "../sessions/" . $_POST['session_folder'] . "/";
	$brain = $_POST["brain"];
	$embr = $_POST["embr"];
	$adult = $_POST["adult"];
	$labs = $_POST["embr_lab"];

if ($brain == "")
	{$brain = "no";}
if ($embr == "")
	{$embr = "false";}
if ($labs == "")
	{$labs = "no";}

test("$data_folder $name_gene $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $brain $embr $labs");

if (!is_dir($session_folder)) {
    mkdir($session_folder);         
}

	$protIDfolder = "$data_folder/ProteinIDlist.txt";
	$contents= file_get_contents($protIDfolder);

	if (strlen($name_gene) <= 100){
		$result = strpos($contents, $name_gene); 
		if ($result !== false){
			if ($adult == "true"){
				$r_cmd = "Rscript superscript.r $data_folder $name_gene $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $brain";
				exec($r_cmd);
			}
			if ($embr == "true"){
				$r_cmd_embr = "Rscript embr.r $data_folder $name_gene $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $labs";			
				exec($r_cmd_embr);
			}
			if ((file_exists($session_folder . $name_gene . ".svg")) && ($adult == "true")){
				$conver_cmd = "./svg2pdf.sh $session_folder$name_gene.svg";
				exec($conver_cmd);
			} else if ($adult == "true") {
				echo "Sorry, the program has fallen on $name_gene adult. Please, report us about it with your request.\n";
			}
			if ((file_exists($session_folder . $name_gene . "_embr.svg")) && ($embr == "true")){
				$conver_cmd = "./svg2pdf.sh $session_folder$name_gene"."_embr.svg";
				exec($conver_cmd);

			} else if ($embr == "true") {

				echo "Sorry, the program has fallen on $name_gene developmental stages. Please, report us about it with your request.\n";
			}
		} else {
			echo 'There is no protein with such ID in the database available.';
			#exec("rm ../figures/temp.svg");
		}
	} else {
		echo "The ID entered is too short.";
	}
?>
