<? 
// ----------------------------������������-------------------------- // 
 
$adminemail="syav.popoff@yandex.ru";  // e-mail ������ 
$adminemail2="travin_birds@mail.ru";  // e-mail ������ 
 
 
$date=date("d.m.y"); // �����.�����.��� 
 
$time=date("H:i"); // ����:������:������� 
 
$backurl="../index.html";  // �� ����� ��������� ��������� ����� �������� ������ 
 
//---------------------------------------------------------------------- // 
 
  
 
// ��������� ������ � ����� 
 
$name=$_POST['name']; 
 
$email=$_POST['email']; 
 
$msg=$_POST['message']; 
 
  
 
// ��������� ���������� e-mail 
 
if (!preg_match("|^([a-z0-9_\.\-]{1,20})@([a-z0-9\.\-]{1,20})\.([a-z]{2,4})|is", 
strtolower($email))) 
 
 { 
 
  echo 
"<center>Try <a 
href='javascript:history.back(1)'><B>agian</B></a>. You wrote a wrong e-mail!"; 
 
  } 
 
 else 
 
 { 
 
 
$msg=" 
 
 
���: $name 
 
 
E-mail: $email
 
 
���������: $msg 
 
 
"; 
 
  
 
 // ���������� ������ ������  
 
mail("$adminemail", "$date $time ��������� 
�� $name", "$msg"); 
 
mail("$adminemail2", "$date $time ��������� 
�� $name", "$msg"); 
 
  
 
// ������� ��������� ������������ 
 
print "<script language='Javascript'><!-- 
function reload() {location = \"$backurl\"}; setTimeout('reload()', 7000); 
//--></script> 
  
 
<p>Yor e-mail was sent! Wait, please. You will be redirected to the main page soon...</p>";  
exit; 
 
 } 
 
?>
