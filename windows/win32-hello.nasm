%macro X 2
        extern  %2
        import  %2 %1
%endmacro

        X       kernel32.dll, ExitProcess
        X       kernel32.dll, GetCommandLineA
        X       user32.dll, MessageBoxA

        segment .data use32
msg:
        db      'Hello, world!'
ec:
        dd      0

        segment .code use32
..start:
        xor     eax, eax
        push    eax
        push    dword msg
        push    dword msg
        push    eax
        call    [MessageBoxA]
        push    dword ec
        call    [ExitProcess]
