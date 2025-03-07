@echo off
setlocal enableextensions enabledelayedexpansion

set cdb_user=couchuser
set cdb_password=couchusersecret
set cdb_server=cdb.yourcompany.com

set dbs=example-db;

for %%d in (%dbs%) do (
    echo == Updating schema for %%d ==
    bin\curl -X POST --data-urlencode schema@schema.json https://%cdb_user%:%cdb_password%@%cdb_server%/%%d/_design/tools/_update/update_schema/schema
    if errorlevel 1 pause
    echo/
)

endlocal
