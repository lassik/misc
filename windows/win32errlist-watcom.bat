wcc386 -q -wx win32errliststructs && wlink op q f win32errliststructs
win32errliststructs.exe > win32errliststructs.h
wcc386 -q -wx win32errlist && wlink op q f win32errlist
win32errlist.exe > win32errlist.txt
