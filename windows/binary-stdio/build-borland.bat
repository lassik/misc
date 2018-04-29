@setlocal
@if not defined BCC set BCC=c:\borland\bcc55
@set PATH=%BCC%\bin;%PATH%
cd "%~dp0"
bcc32 -I"%BCC%\include" -L"%BCC%\lib" binary-stdio.c
