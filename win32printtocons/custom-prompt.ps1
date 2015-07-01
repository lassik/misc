# calling prompt manually behaves as expected
# but when powershell calls prompt internally when rendering the shell prompt, nothing is printed where the prompt text should be
function prompt {
    .\win32printtocons.exe red foo green bar blue baz reset
    Write-Host
    "> "
}
