<?php

session_start();
$_SESSION["progress"]=0;
session_write_close();

header('Content-Type: text/event-stream');
// recommended to prevent caching of event data.
header('Cache-Control: no-cache'); 

function test($d){
	$file = "log.txt";
	file_put_contents($file, $d);
}

	
	$name_gene = trim($_POST['genes']);
	$colorA = $_POST['colorA'];
	$colorB = $_POST['colorB'];
	$colorC = $_POST['colorC'];
	$n_bins = $_POST['bins'];
	$scale = $_POST['scale'];
	$gradient = $_POST['gradient'];
	$data_folder = $_POST['data_folder'];
	$session_folder = "../sessions/" . $_POST['session_folder'] . "/";
	$brain = $_POST['brain'];
	$embr = $_POST["embr"];
	$adult = $_POST["adult"];
	$labs = $_POST["embr_lab"];

if ($brain == "")
	{$brain = "no";}
if ($embr == "")
	{$embr = "false";}
if ($labs == "")
	{$labs = "no";}

test("$data_folder $name_gene $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $embr $labs $adult");

if (!is_dir($session_folder)) {
    mkdir($session_folder);         
}

	$protIDfolder = "$data_folder/ProteinIDlist.txt";
	$contents= file_get_contents($protIDfolder);

$gene_list = explode("\n", $name_gene);
$gene_list_leng = count($gene_list);
$gene_str = str_replace("\n", " ", $name_gene);
$svg_str = "";

$c = 0;
$c_embr = 0;
$total = 0;
foreach ($gene_list as $i)
{
	
	$j = trim($i);
	if (strlen($j) <= 100){
		$result = strpos($contents, $j); 
		if ($result !== false){
			if ($adult == "true"){
				session_start();
				$r_cmd = "Rscript superscript.r $data_folder $j $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $brain";
				exec($r_cmd);
				$c = $c+1;
				$total = $c;
				if ($embr == "true"){
					$total = ($c+$c_embr)/2;
				}
				$_SESSION["progress"]=round(100*$total/$gene_list_leng);
				session_write_close();
			}
			if ($embr == "true"){
				session_start();
				$r_cmd_embr = "Rscript embr.r $data_folder $j $colorA $colorB $colorC $n_bins $gradient $scale $session_folder $labs";
				exec($r_cmd_embr);
				$c_embr = $c_embr+1;
				$total = ($c+$c_embr)/2;
				if ($adult == "false"){
					$total = $c_embr;
				}
				$_SESSION["progress"]=round(100*$total/$gene_list_leng);
				session_write_close();
			}
			$svg_str = $svg_str . $session_folder . $j . ".svg ";
			if ((file_exists($session_folder . $j . ".svg")) && ($adult == "true")){
				echo "$j adult is ready!\n";
			} else if ($adult == "true"){
				echo "Sorry, the program has fallen on $j adult. Please, report us about it with your request.\n";
			}
			if ((file_exists($session_folder . $j . "_embr.svg")) && ($embr == "true")){
				echo "$j developmental stages are ready!\n";
			} else if ($embr == "true") {
				echo "Sorry, the program has fallen on $j developmental stages. Please, report us about it with your request.\n";
			}
		} else {
			echo "There is no protein with $j ID in the database.\n";
			#exec("rm ../figures/temp.svg");
		}
	} else {
		echo "The ID $j entered is too long.\n";
	}
	
}
sleep(1);

?>
