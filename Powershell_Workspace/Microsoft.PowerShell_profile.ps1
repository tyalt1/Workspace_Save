function Add-Path {
	Param([string]$newpath)
	if( $newpath -and -not $env:Path.Contains($newpath) ){ $env:Path +=';' + $newpath }
}

function Show-Path { $ENV:PATH.Split(';') | Sort-Object -Unique }

# Load posh-git example profile
. 'C:\Users\Tyler\Documents\WindowsPowerShell\posh-git\profile.example.ps1'

Set-Alias python 'C:\Python34\python.exe'
Set-Alias erl 'C:\Program Files\erl6.3\bin\erl.exe'
