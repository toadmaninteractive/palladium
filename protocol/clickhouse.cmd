"bin\igorc.exe" -v -erlang -p "igor\clickhouse" -o ..\apps\clickhouse *.igor

if errorlevel 1 pause
