<?php

header('Content-Type: text/event-stream');
// recommended to prevent caching of event data.
header('Cache-Control: no-cache'); 

function test($d){
	$file = "log.txt";
	file_put_contents($file, $d);
}


	$name_gene = trim($_POST['genes']);	
	$is_pdf = $_POST['is_pdf'];
	$session_folder = "../sessions/" . $_POST['session_folder'] . "/";

test("$name_gene $is_pdf $session_folder");

$gene_list = explode(" ", $name_gene);
$svg_str = "";
foreach ($gene_list as $i)
{
	$j = trim($i);
	$conver_cmd = "./svg2pdf.sh " . $session_folder . $j . ".svg";
	exec($conver_cmd);
	$svg_str = $svg_str . $session_folder . $j . ".pdf ";
}

if ($is_pdf == 1){
	$convert_cmd = "pdfunite $svg_str $session_folder" . "output.pdf";
	test($convert_cmd);
	exec($convert_cmd);
} else {
	$zip_cmd = "zip -j $session_folder" . "output.zip " . $svg_str . " tooltip.js";
	exec("rm $session_folder" . "output.zip");
	exec($zip_cmd);	
}



?>
