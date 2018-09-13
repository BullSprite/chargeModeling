
<?php

	set_time_limit(600); 
	file_put_contents('input.txt', '');
	$file = fopen("input.txt","a");
	$myText = $_POST["xCoordFirst"].":"
	.$_POST["yCoordFirst"].":"
	.$_POST["chargeFirst"].":"
	.$_POST["xCoordSecond"].":"
	.$_POST["yCoordSecond"].":"
	.$_POST["chargeSecond"].":"
	.$_POST["xCoordThird"].":"
	.$_POST["yCoordThird"].":"
	.$_POST["chargeThird"].":"
	.$_POST["xCoordShifting"].":"
	.$_POST["yCoordShifting"].":"
	.$_POST["chargeShifting"].":"
	.$_POST["xVelocityShifting"].":"
	.$_POST["yVelocityShifting"].":"
	.($_POST["arrayLen"]*100)."\n";
	
	fwrite($file, $myText);
	fclose($file);
	file_put_contents('result.txt', '');
	//$output =  shell_exec('sudo Rscript Project.r');
	$out = system('Rscript Project.r',$s);
	file_put_contents('o.txt', $output);
	//$cmd  ="C:\WebServers\home\project.ru\www\calculations\s.bat";
	//exec('c:\WINDOWS\system32\cmd.exe /C call C:\WebServers\home\project.ru\www\calculations\s.bat & exit');
	//pclose(popen("start /B ". $cmd, "r"));
	echo file_get_contents("result.txt");
?>
