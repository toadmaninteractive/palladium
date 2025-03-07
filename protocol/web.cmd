"bin\igorc.exe" -v -t ts -x "gen_ts\*.cs" -p "igor\web" -o ..\web\frontend\src\app\protocol *.igor

if errorlevel 1 pause

copy /B /V /Y "ts\igor.ts" "..\web\frontend\src\app\protocol"

if errorlevel 1 pause
