bin\igorc.exe -d -v -p "igor\web" -erl -x gen_erl\*.cs -o ..\apps\web *.igor

if errorlevel 1 pause
