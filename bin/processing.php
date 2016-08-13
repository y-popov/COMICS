<?php
$target_place = date("Y-M-d-B");
$target_dir = "../datasets/" . $target_place . "/";
$pseudo_file = $_POST["pseudo-file"];
if ($pseudo_file == 'null'){
	$target_file = $target_dir . basename($_FILES["file"]["name"]);
	$uploadOk = 1;
	$imageFileType = pathinfo($target_file,PATHINFO_EXTENSION);
	$imageFileType = strtolower($imageFileType);
	$target_file = $target_dir . "inuser." . $imageFileType;
	
	// Check if file already exists
	if (file_exists($target_file)) {
	    echo "Sorry, file already exists. Please, rename it. ";
	    $uploadOk = 0;
	}
	// Check file size
	if ($_FILES["file"]["size"] > 10485760) {
	    echo "Sorry, your file is too large (>10MB). ";
	    $uploadOk = 0;
	}
	// Allow certain file formats
	if($imageFileType != "tsv" && $imageFileType != "xls" && $imageFileType != "xlsx" ) {
	    echo "Sorry, only TSV and Excel files are allowed. ";
	    $uploadOk = 0;
	}
	// Check if $uploadOk is set to 0 by an error
	if ($uploadOk == 0) {
	    echo "Sorry, your file was not uploaded. ";
	// if everything is ok, try to upload file
	} else {
	    mkdir($target_dir, 0777);
	    if (move_uploaded_file($_FILES["file"]["tmp_name"], $target_file)) {
	    	$out_string = "The file ". basename( $_FILES["file"]["name"]). " has been uploaded and preprocessed.";
	    } else {
		echo "Sorry, there was an error uploading your file.";
	    }
	}
}
else {
	$target_file = $target_dir . "inuser.tsv";
	$imageFileType = "tsv";
	mkdir($target_dir, 0777);
	file_put_contents($target_file, $pseudo_file);
	$out_string = "Your data has been uploaded and preprocessed.";;
}

if (file_exists($target_file)) {
	$species = $_POST["fish"];
	$prot_id = str_replace(" ", "\ ", $_POST["protein_id"]);
	$dataset_name = $_POST["data_name"];
	file_put_contents("../datasets.txt", "\n<option value='$target_place'>$dataset_name</option>", FILE_APPEND);
	$dataset_name = str_replace(" ", "\ ", $_POST["data_name"]);
	$r_cmd = "Rscript preprocessing.r $species $imageFileType $target_dir $prot_id $dataset_name > log.txt";
	exec($r_cmd);
	#sleep(5);
	unlink($target_file);
	echo $target_place;
	echo $out_string;
}

?>

