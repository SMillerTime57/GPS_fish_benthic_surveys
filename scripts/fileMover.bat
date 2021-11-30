set FIILELIST=
set FILESPATH=
set DESTPATH=

for /f "delims=" %%x in (%FIILELIST%) do (forfiles /p %FILESPATH% /s /m %%x.* /c "cmd /c copy /y @path %DESTPATH%\@file" 2>>failed_temp.txt)
for /f "tokens=5 " %i in (failed_temp.txt) do (echo.%~i)>>failed_list.txt
del failed_temp.txt