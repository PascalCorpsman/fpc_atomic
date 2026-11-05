<#
  Builds the smart ai and puts it in place of the original ai, to try it in the game.

    .\try_ai.ps1                         build and install into the game folder (default: the repository root)
    .\try_ai.ps1 -GameDir C:\Games\FPCA  another game folder (where atomic_server.exe is)
    .\try_ai.ps1 -Restore                put the original ai.dll back

  The original ai.dll is saved once as ai_original.dll (an existing copy is never overwritten).
  The settings are in ai_smart.ini, see README.md.
#>
param(
  [string]$GameDir = "",
  [string]$Lazbuild = "$env:USERPROFILE\lazarus\4.8\lazbuild.exe",
  [switch]$Restore
)

$ErrorActionPreference = 'Stop'
$Root = Split-Path -Parent $PSScriptRoot
if (-not $GameDir) { $GameDir = $Root }
$GameDir = (Resolve-Path $GameDir).Path
$Ai = Join-Path $GameDir 'ai.dll'
$Backup = Join-Path $GameDir 'ai_original.dll'

if (Get-Process -Name atomic_server -ErrorAction SilentlyContinue) {
  Write-Warning "atomic_server is running: the ai.dll is in use. Close the server first (or unload the ai there with 'u', then load it again with 'a' after this script)."
}

if ($Restore) {
  if (-not (Test-Path $Backup)) { throw "No backup found: $Backup" }
  Copy-Item $Backup $Ai -Force
  Write-Host "Original ai restored: $Ai"
  exit 0
}

if (-not (Test-Path $Lazbuild)) { throw "lazbuild not found: $Lazbuild (use -Lazbuild)" }
Write-Host "Building ai_smart ..."
& $Lazbuild -q (Join-Path $PSScriptRoot 'ai_smart.lpi')
if ($LASTEXITCODE -ne 0) { throw "Build failed (exit code $LASTEXITCODE)" }
$Built = Join-Path $Root 'ai_smart.dll'
if (-not (Test-Path $Built)) { throw "Not found after the build: $Built" }

if ((Test-Path $Ai) -and -not (Test-Path $Backup)) {
  Copy-Item $Ai $Backup
  Write-Host "Original ai saved as $Backup"
}
Copy-Item $Built $Ai -Force
Write-Host "Smart ai installed: $Ai"
Write-Host "The server log shows 'Atomic smart ai' when it is loaded. Back to the original: .\try_ai.ps1 -Restore"

$Local = Join-Path $GameDir 'ai_smart.ini'
if (Test-Path $Local) {
  Write-Host "Settings: $Local (next to the server, wins)"
} else {
  Write-Host "Settings: $(Join-Path $env:LOCALAPPDATA 'fpc_atomic\ai_smart.ini') (written with the defaults on the first start)"
}
