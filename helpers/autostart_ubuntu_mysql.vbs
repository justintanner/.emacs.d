Set WshShell = CreateObject("WScript.Shell")
WshShell.Run "C:\Windows\System32\bash.exe -c ~/autostart.sh",0
Set WshShell = Nothing