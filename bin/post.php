<? 
// ----------------------------конфигурация-------------------------- // 
 
$adminemail="syav.popoff@yandex.ru";  // e-mail админа 
$adminemail2="travin_birds@mail.ru";  // e-mail админа 
 
 
$date=date("d.m.y"); // число.месяц.год 
 
$time=date("H:i"); // часы:минуты:секунды 
 
$backurl="../index.html";  // На какую страничку переходит после отправки письма 
 
//---------------------------------------------------------------------- // 
 
  
 
// Принимаем данные с формы 
 
$name=$_POST['name']; 
 
$email=$_POST['email']; 
 
$msg=$_POST['message']; 
 
  
 
// Проверяем валидность e-mail 
 
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
 
 
Имя: $name 
 
 
E-mail: $email
 
 
Сообщение: $msg 
 
 
"; 
 
  
 
 // Отправляем письмо админу  
 
mail("$adminemail", "$date $time Сообщение 
от $name", "$msg"); 
 
mail("$adminemail2", "$date $time Сообщение 
от $name", "$msg"); 
 
  
 
// Выводим сообщение пользователю 
 
print "<script language='Javascript'><!-- 
function reload() {location = \"$backurl\"}; setTimeout('reload()', 7000); 
//--></script> 
  
 
<p>Yor e-mail was sent! Wait, please. You will be redirected to the main page soon...</p>";  
exit; 
 
 } 
 
?>
